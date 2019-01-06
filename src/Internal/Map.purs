-- | Map functions not present in Data.Map, but which I needed for the 
-- | implementation. Should be opened as a PR on ordered-collections.
module Internal.Map where

import Prelude

import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl)
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple (Tuple(..))

unionsWith :: forall k v f. Ord k => Foldable f => (v -> v -> v) -> f (Map k v) -> Map k v
unionsWith f = foldl (Map.unionWith f) Map.empty

fromSet :: forall k a. Ord k => (k -> a) -> Set k -> Map k a
fromSet f = foldl (\m k -> Map.insert k (f k) m) Map.empty

isSubmapBy :: forall k v x. Ord k => (v -> x -> Boolean) -> Map k v -> Map k x -> Boolean
isSubmapBy f m1 m2 = LL.all f' (Map.toUnfoldable m1 :: LL.List (Tuple k v))
  where
  f' (Tuple k v) = fromMaybe false $ map (f v) (Map.lookup k m2)

mapKeysWith :: forall k0 k1 a. Ord k1 => (a -> a -> a) -> (k0 -> k1) -> Map k0 a -> Map k1 a
mapKeysWith c f = Map.fromFoldableWith c <<< map (lmap f) <<< (Map.toUnfoldable :: Map k0 a -> Array (Tuple k0 a))