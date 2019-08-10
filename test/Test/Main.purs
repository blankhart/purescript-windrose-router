module Test.Main where

import Prelude
import Effect (Effect)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
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
        S "foo" :> CAP "foo" N :> QPs ( welp :: Required String, b :: Required B ) :> VIEW "foo" String
  :<|>  S "bar" :> CAP "bar" B :> QPs ( bar :: Required String ) :> VIEW "bar" String
  :<|>  S "corge" :> CAP "corge" String :> VIEW "corge" String

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

type ReadmeApi page =
       S "profile" :> CAP "username" String :> VIEW "profile" page
  :<|> S "article"
        :> (CAP "id" Int :> VIEW "article_id" page
        :<|> S "search" :> QPs ( term :: Required String ) :> VIEW "article_search" page)

checkReadMeTests :: Effect Unit 
checkReadMeTests = do

  let api = mkRoutable (RouteProxy :: RouteProxy (ReadmeApi String))
    
  let handlers =
        { profile : \username -> "Profile for " <> username
        , article_id : \(id :: Int) -> "Article #" <> show id
        , article_search: \{ term : Required s } -> "Searched for " <> s
        }
  assert $ route api handlers "/profile/blankhart" === Right "Profile for blankhart"

  let links = allLinksWith identity api
  assert $ links.profile "blankhart" === "/profile/blankhart"

  quickCheck $ \username -> 
    route api handlers (links.profile username) === Right (handlers.profile username)

-- Main 

main :: Effect Unit
main = do
  checkPurescriptRoutingTests
  checkReadMeTests
