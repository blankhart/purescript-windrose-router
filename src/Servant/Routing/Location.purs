module Servant.Routing.Location where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Traversable (for)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- 'Location' is used to split the path and query of a relative URI into 
-- components more easily consumed by the router.
--------------------------------------------------------------------------------

newtype Location = Location
  { locPath  :: Array String
  , locQuery :: Array (Tuple String String)
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
      locPath = S.split (S.Pattern "/") $ fromMaybe uriPath (S.stripPrefix (S.Pattern "/") uriPath) 
    locQuery <- 
      if S.null uriQuery 
        then Just []
        else 
          let querySegments = S.split (S.Pattern "&") uriQuery 
          in for querySegments $ \p -> case S.split (S.Pattern "=") p of
            [k, v] -> Just (Tuple k v)
            _ -> Nothing 
    pure $ Location { locPath, locQuery }

    where 

      breakPattern :: S.Pattern -> String -> { before :: String, after :: String }
      breakPattern p@(S.Pattern ps) s = case S.indexOf p s of 
        Nothing -> { before : s, after : "" }
        Just i -> 
          let { before, after } = S.splitAt i s 
          in { before: before, after: S.drop (S.length ps) after }

instance stringFromLocation :: FromLocation String where 
  fromLocation (Location loc) = S.joinWith "/" loc.locPath <> case loc.locQuery of 
    [] -> ""
    pairs -> "?" <> S.joinWith "&" ((\(Tuple k v) -> k <> "=" <> v) <$> pairs)

-- TODO: Instance for NonEmptyString