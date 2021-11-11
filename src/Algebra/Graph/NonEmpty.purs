module Algebra.Graph.NonEmpty (
  -- Algebraic data type for graphs
  Graph (..),
  -- Basic graph construction primitives
  vertex, edge, overlay, connect, vertices, edges, overlays, connects,
  -- Graph folding
  foldg,
  -- Relations on graphs
  isSubgraphOf, (===), structuralEquality,
  -- Graph properties
  size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
  edgeList, vertexSet, edgeSet, adjacencyList,
  -- Standard families of graphs
  -- MISSING: tree, forest, mesh, torus, deBruijn,
  path, circuit, clique, biclique, star, stars, 
  -- Graph transformation
  -- MISSING: sparsify,
  removeVertex, removeEdge, replaceVertex, mergeVertices, splitVertex,
  transpose, induce, simplify,
  -- Graph composition
  box, focus,
  -- Context
  Context (..), context, context_
  -- FocusedGraph
  ) where

import Data.NonEmpty (NonEmpty, tail, (:|))
import Prelude

import Algebra.Graph as G
import Algebra.Graph.Internal (Focus, Hit(..), List, connectFoci, foldr1Safe, overlayFoci, toArray, vertexFocus)
import Algebra.Graph.Internal as Internal
import Algebra.Graph.NonEmpty.AdjacencyMap as AM
import Control.Comonad (class Extend, class Comonad)
import Control.MonadZero (class Alt)
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty as NE
import Data.Set.Monad (Set)
import Data.Set.Monad as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable)
import Prelude as Monad

data Graph a
  = Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)

instance Show a => Show (Graph a) where
  show (Vertex n) = show n
  show (Connect q p) = show q <> "<->" <> show p 
  show (Overlay q p) = show q <> " | " <> show p 

instance eqGraph :: Ord a => Eq (Graph a) where
  eq a b = eq (toAdjacencyMap a) (toAdjacencyMap b)

instance ordGraph :: Ord a => Ord (Graph a) where
  compare a b = compare (toAdjacencyMap a) (toAdjacencyMap b)

instance functorGraph :: Functor Graph where
  map f g = g >>= (vertex <<< f) 

instance applyGraph :: Apply Graph where
  apply = Monad.ap 

instance applicativeGraph :: Applicative Graph where 
  pure = Vertex

instance bindGraph :: Bind Graph where
  bind g f = foldg f Overlay Connect g

instance monadGraph :: Monad Graph

instance altGraph :: Alt Graph where
  alt = Overlay

toUnfoldable :: forall f a. Unfoldable f => Ord a => NonEmpty Set a -> NonEmpty f a
toUnfoldable (s) = case NE.head s, NE.tail s of 
  h, t -> h :| Set.toUnfoldable t

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

toGraph :: forall a. Graph a -> G.Graph a
toGraph (Vertex a) = G.Vertex a
toGraph (Connect x y) = G.Connect (toGraph x) (toGraph y)
toGraph (Overlay x y) = G.Overlay (toGraph x) (toGraph y)

-- | Construct the graph comprising a single isolated vertex. An alias for the
-- | constructor 'Vertex'.
vertex :: forall a. a -> Graph a
vertex = Vertex

-- | Construct the graph comprising a single _directed_ edge.
edge :: forall a. a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)

-- | Overlay two graphs. An alias for the constructor 'Overlay'. This is a
-- | commutative, associative and idempotent operation with the identity 'empty'.
overlay :: forall a. Graph a -> Graph a -> Graph a
overlay = Overlay

-- | Overlay a possibly empty graph (from "Algebra.Graph") with a non-empty
-- graph. If the first argument is 'G.empty', the function returns the second
-- argument; otherwise it is semantically the same as 'overlay'.
-- Complexity: /O(s1)/ time and memory, and /O(s1 + s2)/ size.
--
-- @
--                overlay1 'G.empty' x == x
-- x /= 'G.empty' ==> overlay1 x     y == overlay (fromJust $ toNonEmpty x) y
-- @
overlay1 :: forall a. G.Graph a -> Graph a -> Graph a
overlay1 = maybe identity overlay <<< toNonEmpty

-- | Connect two graphs. An alias for the constructor 'Connect'. This is an
-- | associative operation with the identity 'empty', which distributes over
-- | 'overlay' and obeys the decomposition axiom.
connect :: forall a. Graph a -> Graph a -> Graph a
connect = Connect

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
vertices :: forall a. NonEmpty List a -> Graph a
vertices = overlays <<< map vertex

-- | Construct the graph from an array of edges.
edges :: forall a. NonEmpty List (Tuple a a) -> Graph a
edges = overlays <<< map (uncurry edge)

-- | Overlay a given list of graphs.
overlays :: forall a. NonEmpty List (Graph a) -> Graph a
overlays = concatg overlay

-- | Connect a given list of graphs.
connects :: forall a. NonEmpty List (Graph a) -> Graph a
connects = concatg connect

-- Auxiliary function, similar to 'sconcat'.
concatg :: forall a. (Graph a -> Graph a -> Graph a) -> NonEmpty List (Graph a) -> Graph a
concatg combine (x :| xs) = maybe x (combine x) $ foldr1Safe combine xs

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- | the provided functions to the leaves and internal nodes of the expression.
-- | The order of arguments is: empty, vertex, overlay and connect.
foldg :: forall a b. (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg v o c = go
  where
  go = case _ of 
    Vertex x -> v x
    Overlay x y -> o (go x) (go y)
    Connect x y -> c (go x) (go y)

-- | Returns true if the first graph is a subgraph of the second.
isSubgraphOf :: forall a. Ord a => Graph a -> Graph a -> Boolean
isSubgraphOf x y = overlay x y == y

-- | Structural equality on graph expressions.
--
-- |     x === x           == true
-- |     x === x + 'empty' == false
-- | x + y === x + y       == true
-- | 1 + 2 === 2 + 1       == false
-- | x + y === x * y       == false
structuralEquality :: forall a. Eq a => Graph a -> Graph a -> Boolean
structuralEquality x y = case x, y of
  Vertex x0, Vertex x1 -> x0 == x1
  Overlay x0 y0, Overlay x1 y1 -> x0 === x1 && y0 === y1
  Connect x0 y0, Connect x1 y1 -> x0 === x1 && y0 === y1
  _, _ -> false

infix 4 structuralEquality as ===

-- | The size of a graph, i.e. the number of leaves of the expression
-- | including empty leaves.
size :: forall a. Graph a -> Int
size = foldg (const 1) (+) (+)

-- | Check if a graph contains a given vertex.
hasVertex :: forall a. Eq a => a -> Graph a -> Boolean
hasVertex x = foldg (_ == x) (||) (||)

-- | Check if a graph contains a given edge.
hasEdge :: forall a. Eq a => a -> a -> Graph a -> Boolean
hasEdge s t g = hit g == Edge
  where
  hit (Vertex x) = if x == s then Tail else Miss
  hit (Overlay x y) = case hit x of
    Miss -> hit y
    Tail -> max Tail (hit y)
    Edge -> Edge
  hit (Connect x y) = case hit x of
    Miss -> hit y
    Tail -> if hasVertex t y then Edge else Tail
    Edge -> Edge

-- | The number of vertices in a graph
vertexCount :: forall a. Ord a => Graph a -> Int
vertexCount = (1 + _) <<< Set.size <<< tail <<< vertexSet

-- | The number of edges in a graph.
edgeCount :: forall a. Ord a => Graph a -> Int
edgeCount = AM.edgeCount <<< toAdjacencyMap

-- | The sorted list of vertices in a given graph 
vertexList :: forall a. Ord a => Graph a -> NonEmpty List a 
vertexList = toUnfoldable <<< vertexSet 

-- | The sorted list of edges of a graph.
edgeList :: forall a. Ord a => Graph a -> List (Tuple a a)
edgeList = Internal.toUnfoldable <<< AM.edgeList <<< toAdjacencyMap

-- foldg :: forall a b. (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
-- foldg v o c = go
--   where
--   go = case _ of 
--     Vertex x -> v x
--     Overlay x y -> o (go x) (go y)
--     Connect x y -> c (go x) (go y)

-- append' :: NonEmpty Set a -> NonEmpty Set a -> NonEmpty Set a
-- append' (a :| as) (b :| bs) = a :| Set.insert b (as append bs)

-- | The set of vertices of a given graph.
vertexSet :: forall a. Ord a => Graph a -> NonEmpty Set a
vertexSet = foldg (_ :| mempty) append append

-- | The set of edges of a given graph.
edgeSet :: forall a. Ord a => Graph a -> Set (Tuple a a)
edgeSet = Set.fromFoldable <<< AM.edgeList <<< toAdjacencyMap

-- | The sorted adjacency list of a graph.
adjacencyList :: forall a. Ord a => Graph a -> List (Tuple a (List a))
adjacencyList = AM.adjacencyList <<< toAdjacencyMap

-- | Convert a graph to 'AM.AdjacencyMap'.
toAdjacencyMap :: forall a. Ord a => Graph a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.vertex AM.overlay AM.connect

-- | The path on a list of vertices.
path :: forall a. NonEmpty List a -> Graph a
path (x :| l) = 
  case Array.head (toArray l), Array.tail (toArray l) of 
     Just y, Just ys -> edges ((Tuple x y) :| (Internal.fromFoldable $ Array.zip (y:ys) ys))
     _, _ -> vertex x

-- | The circuit on a list of vertices.
circuit :: forall a. NonEmpty List a -> Graph a
circuit (x :| xs) = path (x :| xs <> pure x)

-- | The clique on a list of vertices.
clique :: forall a. NonEmpty List a -> Graph a
clique = connects <<< map vertex

-- | The biclique on two lists of vertices.
biclique :: forall a. NonEmpty List a -> NonEmpty List a -> Graph a
biclique xs ys = connect (vertices xs) (vertices ys) 

-- | The star formed by a centre vertex connected to a list of leaves.
star :: forall a. a -> List a -> Graph a
star x xs = case Array.head $ Internal.toUnfoldable xs, Array.tail $ Internal.toUnfoldable xs of 
  Just ys, Just yys -> connect (vertex x) (vertices (ys :| Internal.fromFoldable yys))
  _, _ -> vertex x

-- | The stars formed by overlaying a list of stars. An inverse of
-- | adjacencyList.
stars :: forall a. NonEmpty List (Tuple a (List a)) -> Graph a
stars = overlays <<< map (uncurry star)

-- | TODO:
-- | mesh, torus, pairs, deBruijn

-- | Remove a vertex from a given graph.
removeVertex :: forall a. Eq a => a -> Graph a -> Maybe (Graph a)
removeVertex v g = induce (_ /= v) g

-- | Remove an edge from a given graph.
removeEdge :: forall a. Eq a => a -> a -> Graph a -> Graph a
removeEdge s t = filterContext s (_ /= s) (_ /= t)

-- | Filter vertices in a subgraph context.
filterContext :: forall a. Eq a => a -> (a -> Boolean) -> (a -> Boolean) -> Graph a -> Graph a
filterContext s i o g = maybe g go $ context (_ == s) g
  where
  go { inputs, outputs } = 
    G.induce (_ /= s) (toGraph g) 
      `overlay1` transpose (star s (Internal.fromArray $ Array.filter i $ Internal.toArray inputs))
      `overlay` star s (Internal.fromArray $ Array.filter o $ Internal.toArray outputs)


-- | Replaces vertex x with vertex y in a given Graph. If y already exists, 
-- | x and y will be merged.
replaceVertex :: forall a. Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = map \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
mergeVertices :: forall a. (a -> Boolean) -> a -> Graph a -> Graph a
mergeVertices p v = map \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
splitVertex :: forall a. Eq a => a -> NonEmpty List a -> Graph a -> Graph a
splitVertex v us g = g >>= \w -> if w == v then vertices us else vertex w

-- | Transpose a given graph.
transpose :: forall a. Graph a -> Graph a
transpose = foldg Vertex Overlay (flip Connect)

-- | Construct the induced subgraph of a given graph by removing the
-- | vertices that do not satisfy a given predicate.
induce :: forall a. (a -> Boolean) -> Graph a -> Maybe (Graph a)
induce p = foldg
    (\x -> if p x then Just (Vertex x) else Nothing) (k Overlay) (k Connect)
  where
    k _ Nothing  a        = a
    k _ a        Nothing  = a
    k f (Just a) (Just b) = Just (f a b)


-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given expression according to the laws of the algebra.
-- The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
simplify :: forall a. Ord a => Graph a -> Graph a
simplify = foldg Vertex (simple Overlay) (simple Connect)
  where
  simple :: forall g. Eq g => (g -> g -> g) -> g -> g -> g
  simple op x y = do 
    let z = op x y
    case unit of
      _ | x == z -> x
        | y == z -> y
        | otherwise -> z

-- | Compute the Cartesian product of graphs.
box :: forall a b. Graph a -> Graph b -> Graph (Tuple a b)
box x y = overlay (fx <*> y) (fy <*> x)
  where
  fx = foldg (vertex <<< Tuple) overlay overlay x
  fy = foldg (vertex <<< flip Tuple) overlay overlay y

-- 'Focus' on a specified subgraph.
focus :: forall a. (a -> Boolean) -> Graph a -> Focus a
focus f = foldg (vertexFocus f) overlayFoci connectFoci

-- TODO: sparsify

-- | The Context of a subgraph comprises its inputs and outputs, i.e. all
-- the vertices that are connected to the subgraph's vertices. Note that inputs
-- and outputs can belong to the subgraph itself. In general, there are no
-- guarantees on the order of vertices in inputs and outputs; furthermore,
-- there may be repetitions.
type Context a = 
  { inputs :: List a
  , outputs :: List a 
  }

-- | Extract the Context of a subgraph specified by a given predicate. Returns
-- | Nothing if the specified subgraph is empty.
context :: forall a. (a -> Boolean) -> Graph a -> Maybe (Context a)
context p g = case focus p g of
  f | f.ok -> Just { inputs: f.is, outputs: f.os }
  _ -> Nothing

-- | ala fgl context but not returning the vertices
context_ :: forall a. Eq a => Graph a -> a -> Maybe (Context a)
context_ g a = context (a == _) g

instance Extend Graph where
  extend f fg = map f (dup fg)
    where
      dup (Vertex _) = Vertex fg
      dup (Connect p q) = Connect (dup p) (dup q)
      dup (Overlay p q) = Overlay (dup p) (dup q)

instance Comonad Graph where
  extract = go 
    where
      go = case _ of 
        Vertex n -> n
        Connect p _ -> go p
        Overlay p _ -> go p