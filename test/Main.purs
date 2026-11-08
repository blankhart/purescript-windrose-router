module Test.Main where

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodePoints as CP
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Show, Unit, bind, discard, map, show, ($), (-), (<), (<$>), (<<<), (<>), (==), (>), (||))
import Test.QuickCheck (class Arbitrary, class Testable, quickCheck, quickCheck', (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, suchThat)
import Windrose.Router (type (:<|>), type (:>), C, M, P, Q, RouteProxy(..), V, allLinks, mkRoutable, route)

assert :: forall prop. Testable prop => prop -> Effect Unit
assert = quickCheck' 1

-- | Well-formed Unicode text that is heavy in characters a URL must escape.
-- |
-- | QuickCheck's own `String` can contain lone surrogates, which have no
-- | UTF-8 encoding and so cannot survive a trip through a URL.
newtype UrlText = UrlText String

instance showUrlText :: Show UrlText where
  show (UrlText s) = show s

instance arbUrlText :: Arbitrary UrlText where
  arbitrary = UrlText <<< joinWith "" <$> arrayOf piece
    where
    piece :: Gen String
    piece = do
      n <- chooseInt 0 2
      if n == 0 then CP.singleton <$> codePoint
      else map (\i -> fromMaybe "" (Array.index pool i)) (chooseInt 0 (Array.length pool - 1))

    pool :: Array String
    pool =
      [ "/"
      , "?"
      , "&"
      , "="
      , "#"
      , "%"
      , "+"
      , " "
      , "%2F"
      , "%zz"
      , "%FF"
      , "\x00e9"
      , "\x4e2d"
      , "\x1f600"
      , "a"
      , "Z"
      , "0"
      , "-"
      , "."
      ]

    codePoint = map (fromMaybe (codePointFromChar 'x') <<< toEnum) $
      chooseInt 0 0x10FFFF `suchThat` \c -> c < 0xD800 || c > 0xDFFF

-- Main 

main :: Effect Unit
main = do
  log "README tests"
  checkReadMeTests
  log "purescript-routing tests"
  checkPurescriptRoutingTests
  log "Overlapping endpoint tests"
  checkOverlappingTests
  log "Root and optional query tests"
  checkRootAndOptionalQueryTests
  log "Percent-encoding tests"
  checkPercentEncodingTests

-- purescript-routing tests

type MyRoutes =
  P "foo" :> C "foo" Number :> Q (welp :: String, b :: Boolean) :> V "foo"
    :<|> P "bar"
    :> C "bar" Boolean
    :> Q (bar :: String)
    :> V "bar"
    :<|> P "corge"
    :> C "corge" String
    :> V "corge"

foo :: Number -> { welp :: String, b :: Boolean } -> String
foo n { welp, b } =
  "foo = " <> show n <> ", welp: " <> welp <> ", b: " <> show b

bar :: Boolean -> { bar :: String } -> String
bar b { bar: s } = "bar = " <> show b <> ", bar: " <> s

corge :: String -> String
corge c = "corge = " <> c

checkPurescriptRoutingTests :: Effect Unit
checkPurescriptRoutingTests = do
  let
    api = mkRoutable (RouteProxy :: RouteProxy MyRoutes)
    handlers = { foo, bar, corge }
    link = allLinks api

  let handle = route api handlers

  assert $
    handle "foo/12?welp='hi'&b=false"
      === Right "foo = 12.0, welp: 'hi', b: false"

  assert $
    link.foo 12.0 { welp: "'hi'", b: false }
      === "/foo/12.0?welp='hi'&b=false"

  quickCheck $ \(UrlText s) ->
    handle (link.corge s) == Right (corge s)

  quickCheck $ \(Tuple b (UrlText s)) ->
    handle (link.bar b { bar: s }) == Right (bar b { bar: s })

-- README tests

type ReadmeApi =
  P "profile" :> C "username" String :> V "profile"
    :<|> P "article"
    :>
      ( C "id" Int :> V "article_id"
          :<|> P "search"
          :> Q (term :: String)
          :> V "article_search"
      )
    :<|> P "figures"
    :> M "figures" Int
    :> V "figures"

checkReadMeTests :: Effect Unit
checkReadMeTests = do

  let api = mkRoutable (RouteProxy :: _ ReadmeApi)

  let
    handlers =
      { profile: \username -> "Profile for " <> username
      , article_id: \(id :: Int) -> "Article #" <> show id
      , article_search: \{ term } -> "Searched for " <> term
      , figures: \arr -> "Figures: " <> joinWith ", " (show <$> arr)
      }
    match = route api handlers
  assert $ match "/profile/blankhart" === Right "Profile for blankhart"
  assert $ match "/article/34" === Right "Article #34"
  assert $ match "/article/search?term=ptolemy" === Right "Searched for ptolemy"
  assert $ match "/figures/1/2/3/4" === Right "Figures: 1, 2, 3, 4"

  let links = allLinks api
  assert $ links.profile "blankhart" === "/profile/blankhart"
  assert $ links.article_id 34 === "/article/34"
  assert $ links.article_search { term: "ptolemy" } === "/article/search?term=ptolemy"
  assert $ links.figures [ 1, 2, 3, 4 ] === "/figures/1/2/3/4"

  quickCheck $ \(UrlText username) ->
    match (links.profile username) === Right (handlers.profile username)

-- Overlapping endpoint tests

type OverlappingApi =
  P "item" :> C "first" String :> V "first"
    :<|> P "item"
    :> C "second" String
    :> V "second"

checkOverlappingTests :: Effect Unit
checkOverlappingTests = do
  let
    api = mkRoutable (RouteProxy :: _ OverlappingApi)
    -- The second handler is only reached if routing tries the second 
    -- endpoint after the first has already matched.
    handlers =
      { first: \s -> "first: " <> s
      , second: \_ -> unsafeCrashWith "second endpoint was tried after the first matched"
      }
    match = route api handlers
  assert $ match "/item/x" === Right "first: x"

-- Root and optional query tests

type RootApi =
  V "index"
    :<|> Q (page :: Maybe Int)
    :> V "paged"
    :<|> P "posts"
    :> Q (sortBy :: Maybe String, tag :: Array String)
    :> V "posts"

checkRootAndOptionalQueryTests :: Effect Unit
checkRootAndOptionalQueryTests = do
  let
    api = mkRoutable (RouteProxy :: _ RootApi)
    links = allLinks api
    handlers =
      { index: "index"
      , paged: \(q :: { page :: Maybe Int }) -> "paged: " <> show q.page
      , posts: \(q :: { sortBy :: Maybe String, tag :: Array String }) ->
          "posts: " <> show q.sortBy <> " " <> show q.tag
      }
    match = route api handlers

  -- The root prints as "/", with or without a query.
  assert $ links.index === "/"
  assert $ links.paged { page: Just 2 } === "/?page=2"
  assert $ links.paged { page: Nothing } === "/"

  -- A query with nothing in it leaves no "?" behind.
  assert $ links.posts { sortBy: Nothing, tag: [] } === "/posts"
  assert $ links.posts { sortBy: Just "id", tag: [] } === "/posts?sortBy=id"

  assert $ match links.index === Right "index"
  assert $ match (links.posts { sortBy: Nothing, tag: [] })
    === Right (handlers.posts { sortBy: Nothing, tag: [] })
  assert $ match (links.posts { sortBy: Just "id", tag: [ "a", "b" ] })
    === Right (handlers.posts { sortBy: Just "id", tag: [ "a", "b" ] })

-- Percent-encoding tests

type EncodingApi =
  P "signin" :> Q (return :: Maybe String) :> V "signin"
    :<|> P "search"
    :> Q (term :: Maybe String, flag :: Maybe String)
    :> V "search"
    :<|> P "games"
    :> C "id" String
    :> V "game"
    :<|> P "tags"
    :> M "tags" String
    :> Q (one :: String, maybe :: Maybe String, many :: Array String)
    :> V "tags"

checkPercentEncodingTests :: Effect Unit
checkPercentEncodingTests = do
  let
    api = mkRoutable (RouteProxy :: _ EncodingApi)
    links = allLinks api
    handlers =
      { signin: \(q :: { return :: Maybe String }) -> "signin " <> show q.return
      , search: \(q :: { term :: Maybe String, flag :: Maybe String }) ->
          "search " <> show q.term <> " " <> show q.flag
      , game: \(id :: String) -> "game " <> show id
      , tags: \(ts :: Array String) (q :: { one :: String, maybe :: Maybe String, many :: Array String }) ->
          "tags " <> show ts <> " " <> show q.one <> " " <> show q.maybe <> " " <> show q.many
      }
    match = route api handlers

  -- Query names and values are percent-decoded, and `+` is a space.
  assert $ match "/signin?return=%2Fgames%2Fnew" === Right (handlers.signin { return: Just "/games/new" })
  assert $ match "/search?term=two+words" === Right (handlers.search { term: Just "two words", flag: Nothing })
  assert $ match "/search?term=caf%C3%A9" === Right (handlers.search { term: Just "caf\x00e9", flag: Nothing })
  assert $ match "/search?te%72m=x" === Right (handlers.search { term: Just "x", flag: Nothing })

  -- Path segments are percent-decoded after splitting, and `+` is literal.
  assert $ match "/games/caf%C3%A9" === Right (handlers.game "caf\x00e9")
  assert $ match "/games/a%2Fb" === Right (handlers.game "a/b")
  assert $ match "/games/a+b" === Right (handlers.game "a+b")

  -- Only the first `?` and, in each pair, the first `=` are separators.
  assert $ match "/signin?return=/games/new?form=x" === Right (handlers.signin { return: Just "/games/new?form=x" })
  assert $ match "/search?term=a=b" === Right (handlers.search { term: Just "a=b", flag: Nothing })

  -- A bare name has the value "", and empty pairs are skipped.
  assert $ match "/search?flag" === Right (handlers.search { term: Nothing, flag: Just "" })
  assert $ match "/search?term=x&flag" === Right (handlers.search { term: Just "x", flag: Just "" })
  assert $ match "/search?term=x&" === Right (handlers.search { term: Just "x", flag: Nothing })
  assert $ match "/search?&&term=x&&" === Right (handlers.search { term: Just "x", flag: Nothing })

  -- Invalid escapes are kept, and invalid UTF-8 becomes U+FFFD.
  assert $ match "/search?term=%zz" === Right (handlers.search { term: Just "%zz", flag: Nothing })
  assert $ match "/search?term=100%" === Right (handlers.search { term: Just "100%", flag: Nothing })
  assert $ match "/search?term=%FF" === Right (handlers.search { term: Just "\xfffd", flag: Nothing })
  assert $ match "/games/%zz" === Right (handlers.game "%zz")

  -- Links percent-encode captures and query values.
  assert $ links.game "a/b?c" === "/games/a%2Fb%3Fc"
  assert $ match (links.game "a/b?c") === Right (handlers.game "a/b?c")
  assert $ links.search { term: Just "two words&more", flag: Nothing } === "/search?term=two%20words%26more"
  assert $ links.signin { return: Just "/games/new?form=x" } === "/signin?return=%2Fgames%2Fnew%3Fform%3Dx"

  -- Round trips for captures and every kind of query field.
  quickCheck $ \(UrlText s) ->
    match (links.game s) === Right (handlers.game s)

  quickCheck $ \(UrlText s) ->
    let
      q = { return: Just s }
    in
      match (links.signin q) === Right (handlers.signin q)

  quickCheck $ \(Tuple (ts :: Array UrlText) (Tuple (UrlText one) (Tuple (m :: Maybe UrlText) (many :: Array UrlText)))) ->
    let
      unwrap (UrlText s) = s
      tags = map unwrap ts
      q = { one, maybe: map unwrap m, many: map unwrap many }
    in
      match (links.tags tags q) === Right (handlers.tags tags q)
