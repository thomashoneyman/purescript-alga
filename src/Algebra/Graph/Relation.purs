module Algebra.Graph.Relation where

import Algebra.Graph.Internal
import Data.Tuple
import Prelude

import Data.Array (span, uncons)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set(..), union)
import Data.Set as Set
import Data.Unfoldable (class Unfoldable)

data Relation a = Relation {
    -- | The /domain/ of the relation. Complexity: /O(1)/ time and memory.
    domain :: Set a,
    -- | The set of pairs of elements that are /related/. It is guaranteed that
    -- each element belongs to the domain. Complexity: /O(1)/ time and memory.
    relation :: Set (Tuple a a)
  } 
  
empty :: forall a. Relation a
empty = Relation { domain : Set.empty, relation: Set.empty }

vertex :: forall a. a -> Relation a
vertex x = Relation { domain : Set.singleton x, relation: Set.empty }

overlay :: forall a. Ord a => Relation a -> Relation a -> Relation a
overlay (Relation x) (Relation y) = Relation { domain : (x.domain `union` y.domain), relation: (x.relation `union` y.relation)}

connect :: forall a. Ord a => Relation a -> Relation a -> Relation a
connect (Relation x) (Relation y) = Relation 
  { domain: (x.domain `union` y.domain)
  , relation: (x.relation `union` y.relation `union` (x.domain `setProduct` y.domain)) 
  }

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: forall a. Eq a => Relation a -> List (Tuple a (List a))
adjacencyList (Relation r) = go (Set.toUnfoldable r.domain) (Set.toUnfoldable r.relation)
  where
    go :: Array a -> Array (Tuple a a) -> List (Tuple a (List a))
    go arr es = case uncons arr of 
      Nothing -> mempty
      Just {head: x, tail: vs} -> 
        case es == mempty of 
          true -> map ((\x -> Tuple x (fromArray []))) $ fromArray arr
          false ->
            let {init: ys, rest: zs} = span ((x == _) <<< fst) $ es 
            in pure (Tuple x (map snd (fromArray ys))) <> go vs zs


cartesianProduct :: forall a. Ord a => Set a -> Set a -> Set (Tuple a a)
cartesianProduct s1 s2 = Set.fromFoldable $ do
  x <- (Set.toUnfoldable s1 :: List a)
  y <- (Set.toUnfoldable s2 :: List a)
  pure $ Tuple x y

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: forall a f. Foldable f => Ord a => f a -> f a -> Relation a
biclique xs ys = Relation 
  { domain: x `Set.union` y
  , relation: x `cartesianProduct` y
  }
  where
    x = Set.fromFoldable xs
    y = Set.fromFoldable ys

-- | Compute the /symmetric closure/ of a graph.
-- Complexity: /O(m * log(m))/ time.
--
-- @
-- symmetricClosure 'empty'              == 'empty'
-- symmetricClosure ('vertex' x)         == 'vertex' x
-- symmetricClosure ('edge' x y)         == 'edges' [(x,y), (y,x)]
-- symmetricClosure x                  == 'overlay' x ('transpose' x)
-- symmetricClosure . symmetricClosure == symmetricClosure
-- @
symmetricClosure :: forall a. Ord a => Relation a -> Relation a
symmetricClosure (Relation {domain, relation}) = Relation {domain, relation: relation `Set.union` Set.map swap relation}

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: forall a f. Unfoldable f => Relation a -> f a
vertexList (Relation {domain}) = Set.toUnfoldable domain