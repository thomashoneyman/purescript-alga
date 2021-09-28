module Algebra.Graph.Relation.Symmetric where

import Data.Tuple
import Prelude

import Algebra.Graph.Internal (List)
import Algebra.Graph.Relation as R
import Data.Foldable (class Foldable)
import Data.Ord (class Ord)
import Data.Unfoldable (class Unfoldable)

newtype Relation a = SR (R.Relation a)


-- | Extract the underlying symmetric "Algebra.Graph.Relation".
-- Complexity: /O(1)/ time and memory.
--
-- @
-- fromSymmetric ('edge' 1 2)    == 'R.edges' [(1,2), (2,1)]
-- 'R.vertexCount' . fromSymmetric == 'vertexCount'
-- 'R.edgeCount'   . fromSymmetric <= (*2) . 'edgeCount'
-- @
fromSymmetric :: forall a. Relation a -> R.Relation a
fromSymmetric (SR r) = r

toSymmetric :: forall a. Ord a => R.Relation a -> Relation a
toSymmetric = SR <<< R.symmetricClosure



empty :: forall a. Relation a
empty = SR R.empty

vertex :: forall a. a -> Relation a
vertex a = SR (R.vertex a)

overlay :: forall a. Ord a => Relation a -> Relation a -> Relation a
overlay (SR a) (SR b) = SR (R.overlay a b)

connect :: forall a. Ord a => Relation a -> Relation a -> Relation a
connect rx@(SR x) ry@(SR y) = SR (R.connect x y) `overlay` biclique (vertexList ry) (vertexList rx)

biclique :: forall a. Ord a => Array a -> Array a -> Relation a
biclique xs ys = toSymmetric (R.biclique xs ys)

vertexList :: forall a. Relation a -> Array a
vertexList (SR r) = R.vertexList r

adjacencyList :: forall a. Eq a => Relation a -> List (Tuple a (List a))
adjacencyList (SR g) =  R.adjacencyList g