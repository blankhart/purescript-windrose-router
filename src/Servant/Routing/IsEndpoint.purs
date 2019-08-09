module Servant.Routing.IsEndpoint (
  class IsEndpoint
) where 

import Prim.TypeError (class Fail, Text)
import Servant.API (type (:>), kind Route)
import Servant.Routing.API

--------------------------------------------------------------------------------
-- IsEndpoint 
--------------------------------------------------------------------------------

-- | Example:
-- | name :: forall e n . IsEndpoint e n => IsSymbol n => RouteProxy e -> String 
-- | name _ = reflectSymbol (SProxy :: SProxy n)
class IsEndpoint (route :: Route) (name :: Symbol) | route -> name

instance viewIsEndpoint :: IsEndpoint (VIEW name view) name 

else instance seqIsEndpoint 
  :: ( IsEndpoint b s )
  => IsEndpoint (a :> b) s

else instance altIsNoEndpoint
  :: ( Fail (Text "Attempted to determine the name of an API rather than an endpoint."))
  => IsEndpoint (a :<|> b) x

else instance segmentIsNoEndpoint
  :: ( Fail (Text "Endpoints must terminate with a named VIEW.") )
  => IsEndpoint bad x