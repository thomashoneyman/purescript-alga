module Algebra.Graph.Relation where

import Data.Ord (class Ord)
import Data.Set (Set, union)
import Data.Set as Set
import Data.Tuple (Tuple)
import Algebra.Graph.Internal

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
connect (Relation x) (Relation y) = Relation { domain: (x.domain `union` y.domain), 
  relation: (x.relation `union` y.relation `union` (x.domain `setProduct` y.domain)) }