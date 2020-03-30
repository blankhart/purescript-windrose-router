module Windrose.Router.Routable (
  Routable, 
  mkRoutable, 
  class Canonicalize,
  class Distribute,
  class Reassociate,
  EMPTY
) where 

import Prim.TypeError (class Fail, Text, Above)
import Windrose.Router.API (type (:>), type (:<|>), NIL, kind Route, RouteProxy(..))

-- | A wrapper for a token representing the structure of the normalized API.
newtype Routable api = Routable (RouteProxy api)

-- | A smart constructor for a normalized version of the typelevel API
-- | specified by the user.
mkRoutable 
  :: forall nested api . Canonicalize nested api 
  => RouteProxy nested -> Routable api 
mkRoutable _ = Routable (RouteProxy :: RouteProxy api)

-- | An internal combinator used when computing a normalized type.
-- | Users of the library should not refer to this combinator,
-- | even though it must be exported due to transitive export
-- | restrictions.
foreign import data EMPTY :: Route

type BadNestingError = Text "Invalid nesting in the definition of the API.  Endpoints must branch to the right.  Try adjusting parentheses."

-- | Determine a type in which nested combinators have been removed
-- | by distributing the stem of the path over sub-paths.
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

-- TODO: Custom type error if the user passes NIL

-- | Determine a type in which the combinators have 
-- | been rearranged to associate to the right.
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

-- | Determine the canonical type in which combinators have
-- | been unnested and rearranged to associate to the right.
class Canonicalize (from :: Route) (to :: Route) | from -> to

instance routableCanonicalize 
  :: ( Distribute nested EMPTY distributed 
     , Reassociate distributed reassociated
     )
  => Canonicalize nested reassociated
