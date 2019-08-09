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

-- From README

type ExampleApi page =
       S "profile" :> CAP "username" String :> VIEW "profile" page
  :<|> S "article"
        :> (CAP "id" Int :> VIEW "article_id" page
        :<|> S "search" :> QPs ( term :: Required String ) :> VIEW "article_search" page)

-- Test begins

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

main :: Effect Unit
main = do
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

  let 
    readme_api = mkRoutable (RouteProxy :: RouteProxy (ExampleApi String))
    readme_handlers =
      { profile : \username -> "Profile for " <> username
      , article_id : \(id :: Int) -> "Article #" <> show id
      , article_search: \{ term : Required s } -> "Searched for " <> s
      }
    readme_links = allLinksWith identity readme_api

  assert $ route readme_api readme_handlers "/profile/blankhart" === Right "Profile for blankhart"

  assert $ readme_links.profile "blankhart" === "/profile/blankhart"

  quickCheck $ \username -> 
    route readme_api readme_handlers (readme_links.profile username) === Right (readme_handlers.profile username)