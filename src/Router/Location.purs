module Windrose.Router.Location (
  Location(..),
  class FromLocation,
  fromLocation,
  class ToLocation,
  toLocation
) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (for)
import Data.Tuple (Tuple(..))

-- | 'Location' is used to split the path and query of a relative URI into 
-- | components more easily consumed by the router.
newtype Location = Location
  { path  :: Array String
  , query :: Array (Tuple String String)
  }

class ToLocation a where 
  toLocation :: a -> Maybe Location 

class FromLocation a where 
  fromLocation :: Location -> a 

instance locToLocation :: ToLocation Location where 
  toLocation = pure 

instance locFromLocation :: FromLocation Location where 
  fromLocation = identity

instance stringToLocation :: ToLocation String where 
  toLocation postAuthority = do
    let 
      { before: preFragment, after: uriFragment } = breakPattern (S.Pattern "#") postAuthority 
      { before: uriPath, after: uriQuery } = breakPattern (S.Pattern "?") preFragment
      path = S.split (S.Pattern "/") $ fromMaybe uriPath (S.stripPrefix (S.Pattern "/") uriPath) 
    query <- 
      if S.null uriQuery 
        then Just []
        else 
          let querySegments = S.split (S.Pattern "&") uriQuery 
          in for querySegments $ \p -> case S.split (S.Pattern "=") p of
            [k, v] -> Just (Tuple k v)
            _ -> Nothing 
    pure $ Location { path, query }

    where 

      breakPattern :: S.Pattern -> String -> { before :: String, after :: String }
      breakPattern p@(S.Pattern ps) s = case S.indexOf p s of 
        Nothing -> { before : s, after : "" }
        Just i -> 
          let { before, after } = S.splitAt i s 
          in { before: before, after: S.drop (S.length ps) after }

instance nonemptyStringToLocation :: ToLocation NonEmptyString where 
  toLocation = NES.toString >>> toLocation 

instance stringFromLocation :: FromLocation String where 
  fromLocation (Location loc) = S.joinWith "/" loc.path <> case loc.query of 
    [] -> ""
    pairs -> "?" <> S.joinWith "&" ((\(Tuple k v) -> k <> "=" <> v) <$> pairs)