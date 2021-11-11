module Algebra.Graph.Class where

import Algebra.Graph.NonEmpty.Focused (FocusedGraph(..))
import Algebra.Graph.NonEmpty as NEGraph
import Algebra.Graph as Graph

class NEGraph (g :: Type -> Type) where 
  vertex :: forall (a :: Type). a -> g a
  overlay :: forall (a :: Type). g a -> g a -> g a
  connect :: forall (a :: Type). g a -> g a -> g a

class NEGraph g <= Graph (g :: Type -> Type) where 
  empty :: forall (a :: Type). g a

instance NEGraph Graph.Graph where
  vertex = Graph.vertex  
  overlay = Graph.overlay  
  connect = Graph.connect  

instance Graph Graph.Graph where
  empty = Graph.empty  

instance NEGraph FocusedGraph where
  vertex a = FocusedGraph a (NEGraph.vertex a)
  overlay (FocusedGraph a g1) (FocusedGraph _ g2) = FocusedGraph a (NEGraph.overlay g1 g2) 
  connect (FocusedGraph a g1) (FocusedGraph _ g2) = FocusedGraph a (NEGraph.connect g1 g2) 
