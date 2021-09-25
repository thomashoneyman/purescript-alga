module Algebra.Graph.Relation.Symmetric where

import Algebra.Graph.Relation as R
import Data.Ord (class Ord)

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

empty :: forall a. Relation a
empty = SR R.empty

vertex :: forall a. a -> Relation a
vertex a = SR (R.vertex a)

overlay :: forall a. Ord a => Relation a -> Relation a -> Relation a
overlay (SR a) (SR b) = SR (R.overlay a b)

connect :: forall a. Ord a => Relation a -> Relation a -> Relation a
connect (SR a) (SR b) = SR (R.connect a b)