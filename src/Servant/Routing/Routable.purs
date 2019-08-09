module Servant.Routing.Routable (
  Routable, 
  mkRoutable, 
  class Canonicalize,
  class Distribute,
  class Reassociate,
  EMPTY
) where 

import Prim.TypeError (class Fail, Text, Above)
import Servant.API (type (:>), kind Route, RouteProxy(..))
import Servant.Routing.API (type (:<|>), NIL)

--------------------------------------------------------------------------------
-- Routable
--------------------------------------------------------------------------------

newtype Routable api = Routable (RouteProxy api)

mkRoutable 
  :: forall nested api . Canonicalize nested api 
  => RouteProxy nested -> Routable api 
mkRoutable _ = Routable (RouteProxy :: RouteProxy api)

--------------------------------------------------------------------------------
-- Distribute (internal)
--------------------------------------------------------------------------------

foreign import data EMPTY :: Route

type BadNestingError = Text "Invalid nesting in the definition of the API.  Endpoints must branch to the right.  Try adjusting parentheses."

class Distribute (nested :: Route) (prior :: Route) (distributed :: Route) | nested prior -> distributed

instance distributeBadLeftAltCons
  :: ( Fail (Above BadNestingError (Text "Failure of type (a :<|> b) :> c."))
     )
  => Distribute ((a :<|> b) :> c) p d

else instance distributeBadLeftAltAlt
  :: ( Fail (Above BadNestingError (Text "Failure of type (a :<|> b) :<|> c."))
     )
  => Distribute ((a :<|> b) :<|> c) p d

else instance distributePathDist
  :: ( Distribute x p px 
     , Distribute a px pxa 
     , Distribute b px pxb
     )
  => Distribute (x :> (a :<|> b)) p (pxa :<|> pxb)

else instance distributePathSeq
  :: ( Distribute x p px
     , Distribute y px pxy 
     )
  => Distribute (x :> y) p pxy

else instance distributePathAlt
  :: ( Distribute a p pa 
     , Distribute b p pb
     )
  => Distribute (a :<|> b) p (pa :<|> pb)

else instance distributeEMPTY 
  :: Distribute x EMPTY x

else instance distributePrior
  :: Distribute x p (p :> x)

-- | TODO: Custom type error if the user passes NIL

--------------------------------------------------------------------------------
-- Reassociate
--------------------------------------------------------------------------------

class Reassociate (i :: Route) (o :: Route) | i -> o

instance reassociateLeftSeq
  :: ( Reassociate (a :> b :> c) o)
  => Reassociate ((a :> b) :> c) o

else instance reassociateSeq 
  :: ( Reassociate b b' )
  => Reassociate (a :> b) (a :> b')

else instance reassociateLeftAlt 
  :: ( Reassociate (a :<|> b :<|> c) o)
  => Reassociate ((a :<|> b) :<|> c) o

else instance reassociateAltCons 
  :: ( Reassociate a a'
     , Reassociate (b :<|> c) bc'
     )
  => Reassociate (a :<|> (b :<|> c)) (a' :<|> bc')

else instance reassociateAltEnd
  :: ( Reassociate a a' 
     , Reassociate b b'
     )
  => Reassociate (a :<|> b) (a' :<|> b' :<|> NIL)

else instance reassociateSegment
  :: Reassociate a a 

--------------------------------------------------------------------------------
-- Canonicalize (internal)
--------------------------------------------------------------------------------

class Canonicalize (from :: Route) (to :: Route) | from -> to

instance routableCanonicalize 
  :: ( Distribute nested EMPTY distributed 
     , Reassociate distributed reassociated
     )
  => Canonicalize nested reassociated
