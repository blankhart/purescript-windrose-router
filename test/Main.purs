module Test.Main where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (Unit, discard, show, ($), (<$>), (<>), (==))
import Test.QuickCheck (class Testable, quickCheck, quickCheck', (===))
import Windrose.Router (type (:<|>), type (:>), C, M, P, Q, RouteProxy(..), V, allLinks, mkRoutable, route)

assert :: forall prop. Testable prop => prop -> Effect Unit
assert = quickCheck' 1

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

  quickCheck $ \s ->
    handle (link.corge s) == Right (corge s)

  quickCheck $ \(Tuple b s) ->
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

  quickCheck $ \username ->
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
