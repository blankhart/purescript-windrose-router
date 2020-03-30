module Windrose.Router.QueryPairs (
  QueryParams(..), 
  QueryPairs(..), 
  QueryPair, 
  toQueryPairs, 
  fromQueryPairs, 
  FoldFromQueryPairs, 
  FoldToQueryPairs,
  class PartialFromQueryPairs,
  partialFromQueryPairs,
  class PartialToQueryPairs,
  partialToQueryPairs
) where

import Prelude (class Monoid, class Semigroup, bind, const, map, mempty, pure, ($), (<<<), (<>), (==))
import Windrose.Router.UrlPiece (class FromUrlPiece, class ToUrlPiece, fromUrlPiece, toUrlPiece)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, class FoldlRecord, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record.Builder (build, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- QueryPairs represents an array of key-value pairs in a query string.
--------------------------------------------------------------------------------

type QueryPair = Tuple String String 

newtype QueryPairs = QueryPairs (Array QueryPair)

derive newtype instance qpSemigroup :: Semigroup QueryPairs 
derive newtype instance qpMonoid :: Monoid QueryPairs

--------------------------------------------------------------------------------
-- QueryParams represents a record representable as query pairs.
--------------------------------------------------------------------------------

newtype QueryParams r = QueryParams (Record r)

--------------------------------------------------------------------------------
-- 'PartialFromQueryPairs' destructures a 'QueryPairs' based on a record key.
--------------------------------------------------------------------------------

class PartialFromQueryPairs a where 
  partialFromQueryPairs 
    :: String
    -> QueryPairs 
    -> Maybe { valueHead :: a, queryTail :: QueryPairs }

instance maybeExtractQueryParam :: FromUrlPiece a => PartialFromQueryPairs (Maybe a) where 
  partialFromQueryPairs key (QueryPairs array) =
    let
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in
      case splits.yes of
        [] -> Just { valueHead : Nothing, queryTail : QueryPairs splits.no }
        [Tuple _ s] -> do
          v <- fromUrlPiece s
          Just { valueHead : Just v, queryTail : QueryPairs splits.no } 
        _ -> Nothing

else instance arrayExtractQueryParam :: FromUrlPiece a => PartialFromQueryPairs (Array a) where 
  partialFromQueryPairs key (QueryPairs array) =
    let
      -- Uses a key=value rather than key[]=value encoding.
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in do 
      vs <- for splits.yes (fromUrlPiece <<< snd)
      Just { valueHead : vs, queryTail : QueryPairs splits.no }

else instance requiredExtractQueryParam :: FromUrlPiece a => PartialFromQueryPairs a where 
  partialFromQueryPairs key (QueryPairs array) =
    let
      splits = Array.partition (\(Tuple k v) -> k == key) array
    in
      case splits.yes of
        [Tuple _ s] -> do
          v <- fromUrlPiece s
          Just { valueHead : v, queryTail : QueryPairs splits.no } 
        _ -> Nothing

data FoldFromQueryPairs = FoldFromQueryPairs 

instance foldFromQueryPairs 
  ::  ( IsSymbol s
      , PartialFromQueryPairs a
      , Row.Cons s a rin rout
      , Row.Lacks s rin
      )
  =>  FoldingWithIndex 
        FoldFromQueryPairs 
        (SProxy s) 
        (QueryPairs -> Maybe (Record rin)) 
        (Proxy a) 
        (QueryPairs -> Maybe (Record rout)) 
      where 
  foldingWithIndex FoldFromQueryPairs sym acc _ = \pairs -> do 
    { valueHead, queryTail } <- partialFromQueryPairs (reflectSymbol sym) pairs
    prev <- acc queryTail
    pure $ build (insert sym valueHead) prev

fromQueryPairs :: forall params paramsRL 
   . RowToList params paramsRL
  => HFoldlWithIndex 
      FoldFromQueryPairs 
      (QueryPairs -> Maybe (Record ())) 
      (RLProxy paramsRL) 
      (QueryPairs -> Maybe (Record params))
  => QueryPairs
  -> Maybe (Record params)
fromQueryPairs pairs = 
  let empty = const (Just {}) :: QueryPairs -> Maybe (Record ())
  in hfoldlWithIndex FoldFromQueryPairs empty (RLProxy :: _ paramsRL) $ pairs

--------------------------------------------------------------------------------
-- PartialToQueryPairs
--------------------------------------------------------------------------------

class PartialToQueryPairs a where
  partialToQueryPairs :: String -> a -> QueryPairs

toQP :: forall a . ToUrlPiece a => String -> a -> QueryPair
toQP k v = Tuple k (toUrlPiece v)

instance maybeToQueryPairs :: ToUrlPiece a => PartialToQueryPairs (Maybe a) where
  partialToQueryPairs k mv = QueryPairs $ Array.fromFoldable $ map (toQP k) mv

else instance arrayToQueryPairs :: ToUrlPiece a => PartialToQueryPairs (Array a) where
  partialToQueryPairs k vs = QueryPairs $ map (toQP k) vs

else instance otherToQueryPairs :: ToUrlPiece a => PartialToQueryPairs a where
  partialToQueryPairs k v = QueryPairs $ Array.singleton $ toQP k v

data FoldToQueryPairs = FoldToQueryPairs

instance foldToQueryPairs
  ::  ( PartialToQueryPairs a
      , IsSymbol sym
      ) 
  =>  FoldingWithIndex FoldToQueryPairs (SProxy sym) QueryPairs a QueryPairs 
  where
    foldingWithIndex FoldToQueryPairs sym acc a = 
      partialToQueryPairs (reflectSymbol sym) a <> acc

toQueryPairs
  ::  forall params paramsRL 
   .  RowToList params paramsRL
  =>  FoldlRecord FoldToQueryPairs QueryPairs paramsRL params QueryPairs
  =>  QueryParams params
  ->  QueryPairs
toQueryPairs (QueryParams r) = 
  hfoldlWithIndex FoldToQueryPairs (mempty :: QueryPairs) (r :: Record params)