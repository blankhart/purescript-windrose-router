-- Hertz has a manual parser/printer duplex router
-- cf. https://github.com/utkarshkukreti/purescript-hertz/blob/master/examples/Router.purs
-- which is visible at https://hertz-examples.netlify.com/router

module Example.Main where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop) as S
import Effect (Effect)
import Hertz as H
import Hertz.Router (push, replace, subscribe, Mode(..), Url) as R
import Servant.Routing

-- The 'Renderer' is the framework's rendering function.  It is represented
-- here as a data type (a higher-order function) so that the result of 
-- running the router can be cached, and doesn't need to be run on every
-- render.  There is nothing specific to purescript-servant-routing here.
newtype Renderer = Renderer (H.Render Unit State Message)

-- The 'State' is the application state and stores the current URL and  
-- 'Renderer'.  If we weren't caching the 'Renderer' it would only be 
-- necessary to store the URL.
type State = { url :: String, renderer :: Renderer }

-- Rely on the framework to interact with the HTML5 API, and make a simple codec.
newtype UrlWrapper = UrlWrapper R.Url 

instance wrapperToLocation :: ToLocation UrlWrapper where 
  toLocation (UrlWrapper url) = pure $ Location { path: url.path, query: url.query }

-- The only action taken by this application is to change the route.
data Message = Navigate UrlWrapper

-- Our routes are defined through a Servant-style typelevel API.
-- Nested routes are permitted where they make sense, as in this example
-- which branches after "posts".
type ExampleApi =
        VIEW "index"
  :<|>  S "posts" :> (  QPs ( sortBy :: Maybe String ) :> VIEW "postIndex"
                  :<|>  CAP "id" Int :> S "edit" :> VIEW "postEdit")

-- Link and handler records that work with this API have the type (for some 'a')
-- { index :: a, postIndex :: { sortBy :: Maybe String } -> a, postEdit :: Int -> a }
-- Here, the endpoints will return a renderer so that it can be cached.  I.e.,
-- when the router is run against a URL, it returns the rendering function
-- that should be used to draw the application.

-- The dashboard is the basic layout common to all routes in this example.
dashboard :: Array String -> String -> Renderer
dashboard routes description = Renderer $ \self ->
  H.div [] $ [
    H.h2 [] [H.text $ "Url: {" <> self.state.url <> "}"],
    H.h2 [] [H.text $ "Description: " <> description],
    H.div [] $ f self <$> routes
  ]
  where
    f self url = H.h3 [H.class' (if S.drop 1 url == self.state.url then "active" else "")] [
      H.text $ "Sample URL: {" <> url <> "}",
      H.button [H.onClick (\_ -> R.push R.Hash url)] [H.text "Push"],
      H.button [H.onClick (\_ -> R.replace R.Hash url)] [H.text "Replace"]
    ]

-- The component is the entire application in this example.
component :: H.Component Unit
component = H.component "Router" { initialize, update, render, subscriptions }
  where
    -- Canonicalize the typelevel API, which undoes nesting.
    api = mkRoutable (RouteProxy :: _ ExampleApi) 

    -- Define links to the endpoints in the API.
    urls = allLinks api

    -- Define handlers for the endpoints in the API.
    handle description = dashboard routes description
    handlers = 
      { index: 
          handle "index"
      , postEdit: \n -> 
          handle ("edit post #" <> show n)
      , postIndex: \{ sortBy: m } -> 
          handle ("all posts (" <> maybe "unsorted" ("sorting on " <> _) m <> ")")
      }

    -- Define some sample routes to play with, including an error case.
    routes = [
      urls.index,
      urls.postIndex { sortBy: Nothing },
      urls.postIndex { sortBy: Just "id" },
      urls.postIndex { sortBy: Just "title" },
      urls.postEdit 1,
      urls.postEdit 2,
      "nonsense, but try me anyway"
    ]

    -- Start at the "index" endpoint.
    initialize _ = pure { renderer: handlers.index, url: urls.index }
    
    -- On a change in URL, select a new rendering function and cache it in the state.
    -- If an error occurs, cache a function displaying a 404 message instead.
    update self = case _ of 
      Navigate url ->
        self.modify _ 
          { renderer = either (\err -> handle ("Error 404: " <> show err)) identity $ 
              route api handlers url
          , url = maybe "error" fromLocation (toLocation url) 
          }

    -- Use the cached renderer.
    render self = case self.state.renderer of Renderer r -> r self

    -- Rely on the framework's router to notify us of URL changes.
    subscriptions _ = [R.subscribe R.Hash (UrlWrapper >>> Navigate)]

-- There we go.
main :: Effect Unit
main = H.render "main" $ H.make component unit