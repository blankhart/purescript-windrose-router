module Servant.Routing.QueryPairs where

import Prelude
import Servant.API
import Servant.Routing.API

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy, class IsSymbol, reflectSymbol)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Folding (class FoldingWithIndex)
import Prim.Row as Row
import Record.Builder (build, insert)
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- QueryPairs represents an array of key-value pairs in a query string.
--------------------------------------------------------------------------------

newtype QueryPairs = QueryPairs (Array (Tuple String String))

--------------------------------------------------------------------------------
-- 'ExtractQueryParam' destructures a 'QueryPairs' based on a key for a given 
-- functor 'f'.
--------------------------------------------------------------------------------

class ExtractQueryParam f where 
  extractQueryParam 
    :: forall a
     . DecodeQueryParam a 
    => String 
    -> QueryPairs 
    -> Maybe { paramValue :: f a, queryTail :: QueryPairs }

instance maybeExtractQueryParam :: ExtractQueryParam Maybe where 
  extractQueryParam key (QueryPairs array) =
    let
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in
      case splits.yes of
        [] -> Just { paramValue : Nothing, queryTail : QueryPairs splits.no }
        [Tuple _ s] -> do
          v <- decodeQueryParam s
          Just { paramValue : Just v, queryTail : QueryPairs splits.no } 
        _ -> Nothing

instance requiredExtractQueryParam :: ExtractQueryParam Required where 
  extractQueryParam key (QueryPairs array) =
    let
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in
      case splits.yes of
        [Tuple _ s] -> do
          v <- decodeQueryParam s
          Just { paramValue : Required v, queryTail : QueryPairs splits.no } 
        _ -> Nothing

instance arrayExtractQueryParam :: ExtractQueryParam Array where 
  extractQueryParam key (QueryPairs array) =
    let
      -- NOTE: Assumes a key=value rather than key[]=value encoding.
      -- TODO: Make this more flexible.
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in do 
      vs <- for splits.yes (decodeQueryParam <<< snd)
      Just { paramValue : vs, queryTail : QueryPairs splits.no }

--------------------------------------------------------------------------------
-- FoldQueryPairs 
--------------------------------------------------------------------------------

data FoldQueryPairs = FoldQueryPairs 

instance foldQueryPairs 
  ::  ( IsSymbol s
      , ExtractQueryParam f 
      , DecodeQueryParam a 
      , FromCapture a
      , Row.Cons s (f a) rin rout
      , Row.Lacks s rin
      )
  =>  FoldingWithIndex 
        FoldQueryPairs 
        (SProxy s) 
        (QueryPairs -> Maybe (Record rin)) 
        (Proxy (f a)) 
        (QueryPairs -> Maybe (Record rout)) 
      where 
  foldingWithIndex FoldQueryPairs sym acc _ = \pairs -> do 
    { paramValue, queryTail } <- extractQueryParam (reflectSymbol sym) pairs
    prev <- acc queryTail
    pure $ build (insert sym paramValue) prev
