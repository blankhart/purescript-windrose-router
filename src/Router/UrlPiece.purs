module Windrose.Router.UrlPiece (
  class FromUrlPiece, 
  fromUrlPiece, 
  class ToUrlPiece, 
  toUrlPiece
) where 

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Prelude (identity, show, (>=>), (>>>))
import Global as G

--------------------------------------------------------------------------------
-- ToUrlPiece
--------------------------------------------------------------------------------

class ToUrlPiece a where
  toUrlPiece :: a -> String

instance stringToCapture :: ToUrlPiece String where
  toUrlPiece = identity

instance nesToCapture :: ToUrlPiece NonEmptyString where 
  toUrlPiece = NES.toString

instance intToCapture :: ToUrlPiece Int where
  toUrlPiece = show

instance numberToCapture :: ToUrlPiece Number where
  toUrlPiece = show

instance booleanToCapture :: ToUrlPiece Boolean where
  toUrlPiece = case _ of 
    true -> "true"
    false -> "false"

--------------------------------------------------------------------------------
-- FromUrlPiece
--------------------------------------------------------------------------------

class FromUrlPiece a where 
  fromUrlPiece :: String -> Maybe a 

instance fromCaptureString :: FromUrlPiece String where 
  fromUrlPiece = identity >>> Just

instance fromCaptureNonEmptyString :: FromUrlPiece NonEmptyString where 
  fromUrlPiece = NES.fromString >=> Just

instance fromCaptureBoolean :: FromUrlPiece Boolean where 
  fromUrlPiece = case _ of 
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing 

instance fromCaptureInt :: FromUrlPiece Int where 
  fromUrlPiece = Int.fromString

instance fromCaptureNumber :: FromUrlPiece Number where 
  fromUrlPiece = G.readFloat >>> case _ of 
    n | G.isFinite n -> Just n
    _ -> Nothing
    