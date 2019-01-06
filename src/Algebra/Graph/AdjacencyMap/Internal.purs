-- | The 'AdjacencyMap' data type represents a graph by a map of vertices to
-- | their adjacency sets.
module Algebra.Graph.AdjacencyMap.Internal where

import Prelude

import Algebra.Graph.Internal (List, toArray)
import Control.Fold as Fold
import Data.Array (unzip)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Internal.Map as IMap

newtype AdjacencyMap a = AM (Map a (Set a))

derive instance newtypeAdjacencyMap :: Newtype (AdjacencyMap a) _
derive instance eqAdjacencyMap :: Eq a => Eq (AdjacencyMap a)

instance ordAdjacencyMap :: Ord a => Ord (AdjacencyMap a) where
  compare (AM x) (AM y) = Fold.foldl Fold.mconcat
    [ compare (Map.size x) (Map.size y) 
    , compare (Map.keys x) (Map.keys y)
    , compare (num x) (num y)
    , compare x y
    ]
    where
    num = Fold.foldl Fold.sum <<< map Set.size

-- TODO: What to do about `one`? Surely this is not law-abiding. Should this
-- instance exist? It's only in order to use (+) and (*) as is done in -alga
instance semiringAdjacencyMap :: Ord a => Semiring (AdjacencyMap a) where
  add (AM x) (AM y) = AM $ Map.unionWith Set.union x y
  mul (AM x) (AM y) = AM $ IMap.unionsWith Set.union [ x, y, IMap.fromSet (const $ Map.keys y) (Map.keys x) ]
  zero = AM Map.empty
  one = AM Map.empty

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
-- /Note: this function is for internal use only/.
consistent :: forall a. Ord a => AdjacencyMap a -> Boolean
consistent (AM m) = Set.subset (referredToVertexSet m) (Map.keys m)

-- | The list of edges of an adjacency map.
-- /Note: this function is for internal use only/.
internalEdgeList :: forall a. Map a (Set a) -> List (Tuple a a)
internalEdgeList m = do
  Tuple x ys <- Map.toUnfoldable m
  y <- Set.toUnfoldable ys
  pure $ Tuple x y

-- | The set of vertices that are referred to by the edges of an adjacency map.
-- /Note: this function is for internal use only/.
referredToVertexSet :: forall a. Ord a => Map a (Set a) -> Set a
referredToVertexSet = Set.fromFoldable <<< uncurry append <<< unzip <<< toArray <<< internalEdgeList
  