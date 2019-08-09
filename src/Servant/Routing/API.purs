module Servant.Routing.API where 

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Global as G
import Prelude ((>>>), (<<<), identity, otherwise)
import Servant.API (kind Route)

--------------------------------------------------------------------------------
-- ALT (combinator)
--------------------------------------------------------------------------------

foreign import data ALT :: Route -> Route -> Route
infixr 3 type ALT as :<|>

--------------------------------------------------------------------------------
-- VIEW (combinator)
--------------------------------------------------------------------------------

-- | The type parameter to 'VIEW' would typically be a functor unique to a 
-- | client framework used to display HTML, such as the 'View Action' type in 
-- | various Elm-likes.  That is, the combinator to use in building the API would 
-- | (or could) be something like 'Page (View Action)'. 
-- | TODO: Custom type error if Page :> _ or if QPs :> a and a /~ Page
foreign import data VIEW :: Symbol -> Type -> Route 

--------------------------------------------------------------------------------
-- NIL (internal combinator)
--------------------------------------------------------------------------------

-- | Users of the library should not use this combinator.
-- TODO: Possibly eliminate and replace with a new kind just of canonical paths.
-- Existing setup relies on instance chains and is not kind-safe. 
foreign import data NIL :: Route

--------------------------------------------------------------------------------
-- FromUriData
--------------------------------------------------------------------------------

-- TODO: FromCapture and DecodeQueryParam generally should have the same 
-- representation, and so should be combined.

--------------------------------------------------------------------------------
-- FromCapture
--------------------------------------------------------------------------------

class FromCapture a where 
  fromCapture :: String -> Maybe a 

instance fromCaptureString :: FromCapture String where 
  fromCapture = Just <<< identity 

-- TODO: NonEmptyString instance.

instance fromCaptureBoolean :: FromCapture Boolean where 
  fromCapture = case _ of 
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing 

instance fromCaptureInt :: FromCapture Int where 
  fromCapture = Int.fromString

instance fromCaptureNumber :: FromCapture Number where 
  fromCapture = G.readFloat >>> check
    where
      check num | G.isFinite num = Just num
                | otherwise    = Nothing

--------------------------------------------------------------------------------
-- DecodeQueryParam
--------------------------------------------------------------------------------

class DecodeQueryParam a where
  decodeQueryParam :: String -> Maybe a

instance decodeQueryParamFromCapture :: FromCapture a => DecodeQueryParam a where 
  decodeQueryParam = fromCapture
