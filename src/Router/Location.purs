module Windrose.Router.Location
  ( Location(..)
  , class FromLocation
  , fromLocation
  , class ToLocation
  , toLocation
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Windrose.Router.PercentEncoding (percentDecode, percentEncode)

-- | 'Location' is used to split the path and query of a relative URI into 
-- | components more easily consumed by the router.  The path segments and
-- | query names and values are decoded text, without percent-escapes.
newtype Location = Location
  { path :: Array String
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

-- | Parses the path, query and fragment of a URL, such as
-- | `/games/caf%C3%A9?return=%2Fhome&flag#top`.  This instance never fails.
-- |
-- | The fragment is dropped.  The path is split on `/` and each segment is
-- | then percent-decoded, so `%2F` stays inside its segment and `+` is
-- | literal.  The query is read as `application/x-www-form-urlencoded`, as
-- | `URLSearchParams` does: it is split on `&`, empty pairs are skipped,
-- | each pair is split on its first `=` (a bare `flag` has the value `""`),
-- | and `+` is read as a space before percent-decoding.  An invalid escape
-- | such as `%zz` is kept literally.
instance stringToLocation :: ToLocation String where
  toLocation postAuthority =
    let
      { before: preFragment } = breakPattern (S.Pattern "#") postAuthority
      { before: uriPath, after: uriQuery } = breakPattern (S.Pattern "?") preFragment
      path = map percentDecode $ S.split (S.Pattern "/") $
        fromMaybe uriPath (S.stripPrefix (S.Pattern "/") uriPath)
      query = map queryPair $ Array.filter (not <<< S.null) $
        S.split (S.Pattern "&") uriQuery
    in
      Just $ Location { path, query }

    where

    queryPair :: String -> Tuple String String
    queryPair segment =
      let
        { before, after } = breakPattern (S.Pattern "=") segment
      in
        Tuple (decodeQueryPiece before) (decodeQueryPiece after)

    decodeQueryPiece :: String -> String
    decodeQueryPiece =
      S.replaceAll (S.Pattern "+") (S.Replacement " ") >>> percentDecode

    breakPattern :: S.Pattern -> String -> { before :: String, after :: String }
    breakPattern p@(S.Pattern ps) s = case S.indexOf p s of
      Nothing -> { before: s, after: "" }
      Just i ->
        let
          { before, after } = S.splitAt i s
        in
          { before: before, after: S.drop (S.length ps) after }

instance nonemptyStringToLocation :: ToLocation NonEmptyString where
  toLocation = NES.toString >>> toLocation

instance stringFromLocation :: FromLocation String where
  fromLocation (Location loc) = S.joinWith "/" (percentEncode <$> loc.path) <> case loc.query of
    [] -> ""
    pairs -> "?" <> S.joinWith "&" ((\(Tuple k v) -> percentEncode k <> "=" <> percentEncode v) <$> pairs)