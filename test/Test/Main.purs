module Test.Main where

import Prelude
import Effect (Effect)
import Data.Either (Either(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import Servant.API
import Servant.Routing
import Test.StrongCheck

-- Quick fix for missing ToCapture instances in purescript-servant

newtype N = N Number 

instance nToCapture :: ToCapture N where 
  toCapture (N n) = show n

derive newtype instance nFromCapture :: FromCapture N
derive newtype instance nArbitrary :: Arbitrary N

newtype B = B Boolean 

instance bToCapture :: ToCapture B where 
  toCapture (B b) = show b

derive newtype instance bEncodeQueryParam :: EncodeQueryParam B
derive newtype instance bFromCapture :: FromCapture B
derive newtype instance bArbitrary :: Arbitrary B

-- purescript-routing tests begin

type MyRoutes = 
        S "foo" :> CAP "foo" N :> QPs ( welp :: Required String, b :: Required B ) :> VIEW "foo"
  :<|>  S "bar" :> CAP "bar" B :> QPs ( bar :: Required String ) :> VIEW "bar"
  :<|>  S "corge" :> CAP "corge" String :> VIEW "corge"

foo :: N -> { welp :: Required String, b :: Required B } -> String 
foo (N f) { welp : Required w, b : Required (B b) } = 
  "foo = " <> show f <> ", welp: " <> w <> ", b: " <> show b

bar :: B -> { bar :: Required String } -> String
bar (B b) { bar : Required s } = "bar = " <> show b <> ", bar: " <> s 

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
    link.foo (N 12.0) { welp: Required "'hi'", b: Required (B false) }
      === "/foo/12.0?welp='hi'&b=false"

  quickCheck $ \s -> 
    handle (link.corge s) == Right (corge s)
  
  quickCheck $ \(Tuple b s) -> 
    handle (link.bar b { bar: Required s }) == Right (bar b { bar: Required s })

-- README tests

type ReadmeApi =
       S "profile" :> CAP "username" String :> VIEW "profile"
  :<|> S "article"
        :> (CAP "id" Int :> VIEW "article_id"
        :<|> S "search" :> QPs ( term :: Required String ) :> VIEW "article_search")
  :<|> S "figures" :> CAPMANY "figures" Int :> VIEW "figures"

checkReadMeTests :: Effect Unit 
checkReadMeTests = do

  let api = mkRoutable (RouteProxy :: _ ReadmeApi)

  let handlers =
        { profile : \username -> "Profile for " <> username
        , article_id : \(id :: Int) -> "Article #" <> show id
        , article_search: \{ term : Required s } -> "Searched for " <> s
        , figures: \arr -> "Figures: " <> joinWith ", " (show <$> arr)
        }
      match = route api handlers
  assert $ match "/profile/blankhart" === Right "Profile for blankhart"
  assert $ match "/article/34" === Right "Article #34"
  assert $ match "/article/search?term=ptolemy" === Right "Searched for ptolemy"
  assert $ match "/figures/1/2/3/4" === Right "Figures: 1, 2, 3, 4"

  let links = allLinksWith identity api
  assert $ links.profile "blankhart" === "/profile/blankhart"
  assert $ links.article_id 34 === "/article/34"
  assert $ links.article_search { term: Required "ptolemy" } === "/article/search?term=ptolemy"
  assert $ links.figures [1, 2, 3, 4] === "/figures/1/2/3/4"

  quickCheck $ \username -> 
    route api handlers (links.profile username) === Right (handlers.profile username)

-- Main 

main :: Effect Unit
main = do
  log "README tests"
  checkReadMeTests
  log "purescript-routing tests"
  checkPurescriptRoutingTests
  