-- | Percent-encoding for path segments and query names and values.
-- |
-- | Decoding follows the WHATWG URL standard, so it never fails: a `%` that
-- | is not followed by two hex digits is kept literally, and escaped bytes
-- | that are not valid UTF-8 decode to U+FFFD.
module Windrose.Router.PercentEncoding
  ( percentEncode
  , percentDecode
  ) where

-- | Percent-encode everything except `A-Z a-z 0-9 - _ . ! ~ * ' ( )`, as
-- | `encodeURIComponent` does.  A lone surrogate, which has no UTF-8
-- | encoding, is encoded as U+FFFD.
foreign import percentEncode :: String -> String

-- | Decode every well-formed `%XX` escape as UTF-8.  Does not treat `+`
-- | specially; query parsing turns `+` into a space before decoding.
foreign import percentDecode :: String -> String
