module Algebra.Graph.NonEmpty.Pointed where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Control.Comonad (class Comonad, class Extend, extend)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Tuple (Tuple(..), uncurry)
import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.Internal (List, foldr1Safe, toArray)
import Algebra.Graph.Internal as Internal
import Algebra.Graph (Graph(..))
import Algebra.Graph as G

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen, oneOf)
import Control.Monad.Gen.Class as Gen
import Control.Lazy

data PointedGraph n
  = PVertex n
  | PConnect (PointedGraph n) (Graph n) 
  | POverlay (PointedGraph n) (Graph n)

derive instance Ord a => Ord (PointedGraph a)
derive instance (Eq a, Ord a) => Eq (PointedGraph a)
derive instance Functor PointedGraph

instance Show a => Show (PointedGraph a) where
  show (PVertex n) = ">" <> show n <> "<"
  show (PConnect q p) = show q <> "<->" <> show p 
  show (POverlay q p) = show q <> " | " <> show p 

-- instance Arbitrary a => Arbitrary (PointedGraph a) where
--   arbitrary = fix genGraph

-- genGraph :: forall a. Arbitrary a => Gen (PointedGraph a) -> Gen (PointedGraph a)
-- genGraph gr = Gen.resize (min 5) $ Gen.sized genGraph'
--   where
--     genGraph' :: Int -> Gen (PointedGraph a) 
--     genGraph' s
--       | s > 1 = Gen.resize (_ - 1) $ oneOf $ NEA.fromNonEmpty (v :| r gr)
--       | otherwise = v

--     v = PVertex <$> arbitrary

--     r :: Gen (PointedGraph a) -> Array (Gen (PointedGraph a))
--     r g = [ PConnect <$> g <*> g
--           , POverlay <$> g <*> g
--           ]

-- This should work but needs more thought
-- toNEGraph :: forall n. PointedGraph n -> NE.Graph n
-- toNEGraph (PVertex a) = NE.Vertex a
-- toNEGraph (PConnect p q) = NE.Connect (toNEGraph p) (toGraph q)
-- toNEGraph (POverlay p q) = NE.Overlay (toNEGraph p) (toGraph q)

toGraph :: forall n. PointedGraph n -> Graph n
toGraph (PVertex a) = Vertex a
toGraph (PConnect p q) = Connect (toGraph p) q
toGraph (POverlay p q) = Overlay (toGraph p) q

instance Extend PointedGraph where
  extend f fg = map f (dup fg)
    where
      -- duplicate :: forall a. PointedGraph a -> PointedGraph (PointedGraph a)
      dup (PVertex _) = PVertex fg
      dup (PConnect p q) = PConnect (dup p) (extend (const fg) q)
      dup (POverlay p q) = POverlay (dup p) (extend (const fg) q)

-- PConnect (PVertex true) (Vertex false)
-- PConnect (PVertex (PConnect (PVertex true) (Vertex false))) (Vertex (PConnect (PVertex true) (Vertex false))))

instance Comonad PointedGraph where
  extract = go 
    where
      go = case _ of 
        PVertex n -> n
        PConnect p _ -> go p
        POverlay p _ -> go p

-- | Convert an algebraic graph (from "Algebra.Graph") into a non-empty
-- algebraic graph. Returns 'Nothing' if the argument is 'G.empty'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- toNonEmpty 'G.empty'       == Nothing
-- toNonEmpty ('T.toGraph' x) == Just (x :: 'Graph' a)
-- @
toNonEmpty :: forall a. G.Graph a -> Maybe (Graph a)
toNonEmpty = G.foldg Nothing (Just <<< Vertex) (go Overlay) (go Connect)
  where
    go _ Nothing  y        = y
    go _ x        Nothing  = x
    go f (Just x) (Just y) = Just (f x y)

-- | Construct the graph comprising a single isolated vertex. An alias for the
-- | constructor 'Vertex'.
vertex :: forall a. a -> PointedGraph a
vertex = PVertex

-- | Construct the graph comprising a single _directed_ edge.
edge :: forall a. a -> a -> PointedGraph a
edge x y = connect1 (vertex x) (G.vertex y)

-- | Overlay two graphs. An alias for the constructor 'Overlay'. This is a
-- | commutative, associative and idempotent operation with the identity 'empty'.
overlay :: forall a. PointedGraph a -> PointedGraph a -> PointedGraph a
overlay p q = POverlay p (toGraph q)

overlay1 :: forall a. PointedGraph a -> G.Graph a -> PointedGraph a
overlay1 = POverlay


-- | Connect two graphs. An alias for the constructor 'Connect'. This is an
-- | associative operation with the identity 'empty', which distributes over
-- | 'overlay' and obeys the decomposition axiom.
connect :: forall a. PointedGraph a -> PointedGraph a -> PointedGraph a
connect p q = PConnect p (toGraph q)

connect1 :: forall a. PointedGraph a -> G.Graph a -> PointedGraph a
connect1 = PConnect

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices1 [x]           == 'vertex' x
-- 'hasVertex' x . vertices1 == 'elem' x
-- 'vertexCount' . vertices1 == 'length' . 'Data.List.NonEmpty.nub'
-- 'vertexSet'   . vertices1 == Set.'Set.fromList' . 'Data.List.NonEmpty.toList'
-- @
vertices :: forall a. NonEmpty List a -> PointedGraph a
vertices = overlays <<< map vertex

-- | Construct the graph from an array of edges.
edges :: forall a. NonEmpty List (Tuple a a) -> PointedGraph a
edges = overlays <<< map (uncurry edge)

-- | Overlay a given list of graphs.
overlays :: forall a. NonEmpty List (PointedGraph a) -> PointedGraph a
overlays = concatg overlay

-- | Connect a given list of graphs.
connects :: forall a. NonEmpty List (PointedGraph a) -> PointedGraph a
connects = concatg connect

-- | The path on a list of vertices.
path :: forall a. NonEmpty List a -> PointedGraph a
path (x :| l) = 
  case Array.head (toArray l), Array.tail (toArray l) of 
     Just y, Just ys -> edges ((Tuple x y) :| (Internal.fromFoldable $ Array.zip (y:ys) ys))
     _, _ -> vertex x

-- | The circuit on a list of vertices.
circuit :: forall a. NonEmpty List a -> PointedGraph a
circuit (x :| xs) = path (x :| xs <> pure x)

-- Auxiliary function, similar to 'sconcat'.
concatg :: forall a. (PointedGraph a -> PointedGraph a -> PointedGraph a) -> NonEmpty List (PointedGraph a) -> PointedGraph a
concatg combine (x :| xs) = maybe x (combine x) $ foldr1Safe combine xs

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- | the provided functions to the leaves and internal nodes of the expression.
-- | The order of arguments is: empty, vertex, overlay and connect.
foldg :: forall a b. (a -> b) -> b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> PointedGraph a -> b
foldg p e v o c = go
  where
  go = case _ of 
    PVertex x -> p x
    POverlay x y -> o (go x) (G.foldg e v o c y)
    PConnect x y -> c (go x) (G.foldg e v o c y)

-- | The sorted adjacency list of a graph.
adjacencyList :: forall a. Ord a => PointedGraph a -> List (Tuple a (List a))
adjacencyList = AM.adjacencyList <<< toAdjacencyMap

-- | Convert a graph to 'AM.AdjacencyMap'.
toAdjacencyMap :: forall a. Ord a => PointedGraph a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.vertex AM.empty AM.vertex AM.overlay AM.connect
