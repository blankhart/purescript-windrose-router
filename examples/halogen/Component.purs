-- cf. https://github.com/thomashoneyman/purescript-halogen-realworld/blob/master/src/Component/Router.purs

module Example.Halogen.Component where

import Prelude

import Data.Array as A
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Windrose.Router (type (:<|>), type (:>), P, V)
import Windrose.Router as W

type ExampleApi =
        V "index"
  :<|>  P "link-a" :> V "linkA" 
  :<|>  P "link-b" :> V "linkB"
  :<|>  P "link-c" :> V "linkC"

-- Canonicalize the typelevel API, which undoes nesting.
-- This should be unnecessary in this case because the 
-- sample API is simple and already in canonical form.
-- Although the type signature is provided, it can be inferred
-- at the cost of a compiler warning.
api :: forall api 
  .  W.Canonicalize ExampleApi api 
  => W.Routable api
api = W.mkRoutable (W.RouteProxy :: W.RouteProxy ExampleApi) 

-- Define links to the endpoints in the API.
-- The type signature is provided for illustration.
links :: forall api links 
  .  W.Canonicalize ExampleApi api 
  => W.HasLinks api W.Link links 
  => links
links = W.allLinks api


{-
The type of the handler record cannot be specified in the same manner 
as the type of the links record, i.e. as:

handlers :: forall api handlers 
  .  W.Canonicalize ExampleApi api 
  => W.HasRouter api String handlers 
  => handlers

That is, attempting to do this will produce this compiler error: 

  Could not match type

      { index :: String
      , linkA :: String
      , linkB :: String
      , linkC :: String
      }

    with type

      handlers0

The functional dependency constraints in the type signature ensure
that these types are in fact equal, but the Purescript compiler 
will not unify a rigid type variable with any type other than itself.
More specifically, `handlers` is a rigid type variable because it is 
behind a `forall`.  The Purescript compiler constructs `handlers0`  
as a placeholder type to stand in for the concrete type that it expects
the caller to specify.  Although the constraints will not allow a caller
to use any type other than the record type above, the compiler's type
checker does not include machinery that would recognize and allow this.
(Explanation credit to Nate Faubion; any mistakes here are my own.)

The compiler can infer the type of the handler record if it is left 
unspecified.  This will result in a compiler warning if the record is 
defined at the  top level, though not if it is defined as a `let` binding
inside a function.  Usually the handler record will have only one 
usage site, however, as in this example.
-}
handlers :: 
  { index :: String
  , linkA :: String
  , linkB :: String
  , linkC :: String
  }
handlers =
  { index: "Index (no hash)"
  , linkA: "Link A"
  , linkB: "Link B"
  , linkC: "Link C"
  }

type Slot = H.Slot Query Void

data Query a = ChangeRoute String a

-- We cache the rendering function selected by the router 
-- on each hash change (in `handleQuery`, below) to avoid 
-- re-parsing the route on each render.
--
-- This is perhaps a lightweight solution compared to using a separate 
-- Halogen routing component as the root, and having the 
-- parent cache and dispatch on some representation of the route.
-- 
-- We cannot use a type synonym if we cache the rendering function
-- because the `render` type signature refers to `State` and 
-- circular type synonyms are illegal.
type Renderer = forall act m. State -> H.ComponentHTML act () m

newtype State = State
  { renderer :: Renderer 
  , history :: Array String 
  }

component :: forall i o m. H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState: \_ -> State { renderer: render handlers.index, history: [] }
    , render: \state@(State st) -> st.renderer state
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

render :: forall act m. String -> State -> H.ComponentHTML act () m
render page (State state) =
  let 
    frag hash = HP.href $ "#" <> hash
  in 
    HH.div_
      [ HH.h1_ [ HH.text $ "Viewing: " <> page ]
      , HH.p_ [ HH.text "Change the URL hash or choose an anchor link..." ]
      , HH.ul_
          [ HH.li_ [ HH.a [ frag links.linkA ] [ HH.text "Link A" ] ]
          , HH.li_ [ HH.a [ frag links.linkB ] [ HH.text "Link B" ] ]
          , HH.li_ [ HH.a [ frag links.linkC ] [ HH.text "Link C" ] ]
          , HH.li_ [ HH.a [ frag "link-d"    ] [ HH.text "Link D (does not exist)" ] ]
          ]
      , HH.p_ [ HH.text "...to see the history logged below:" ]
      , HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.history
      ]

error404 :: forall act m. String -> State -> H.ComponentHTML act () m
error404 err _ = 
  HH.div_
    [ HH.p_ [ HH.text "Error 404" ]
    , HH.p_ [ HH.text err ]
    ]

handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
  ChangeRoute path a -> do
    H.modify_ \(State st) -> State
      { renderer: either (show >>> error404) render (W.route api handlers path)
      , history: st.history `A.snoc` path 
      }
    pure (Just a)