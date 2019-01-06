-- |
-- | Missing: tree, forest
module Algebra.Graph (
  -- Algebraic data type for graphs
  Graph (..),
  -- Basic graph construction primitives
  empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,
  -- Graph folding
  foldg,
  -- Relations on graphs
  isSubgraphOf, (===), structuralEquality,
  -- Graph properties
  isEmpty, size, hasVertex, hasEdge, vertexCount, edgeCount, vertexList,
  edgeList, vertexSet, edgeSet, adjacencyList,
  -- Standard families of graphs
  -- MISSING: tree, forest, mesh, torus, deBruijn,
  path, circuit, clique, biclique, star, stars, 
  -- Graph transformation
  -- MISSING: sparsify,
  removeVertex, removeEdge, replaceVertex, mergeVertices, splitVertex,
  transpose, induce, simplify,
  -- Graph composition
  compose, box,
  -- Context
  Context (..), context
  ) where

import Prelude

import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.Internal (Focus, Hit(..), List, connectFoci, emptyFocus, fromArray, overlayFoci, toArray, vertexFocus)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Control.MonadZero (class Alt, class Alternative, class Plus, guard)
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (foldl1)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Internal.Set as ISet
import Prelude as Monad

data Graph a
  = Empty
  | Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)

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
  bind g f = foldg Empty f Overlay Connect g

instance monadGraph :: Monad Graph

instance altGraph :: Alt Graph where
  alt = Overlay

instance plusGraph :: Plus Graph where
  empty = Empty

instance alternativeGraph :: Alternative Graph

instance monadZeroGraph :: MonadZero Graph

instance monadPlusGraph :: MonadPlus Graph

-- | Construct the empty graph. An alias for the constructor 'Empty'.
empty :: forall a. Graph a
empty = Empty

-- | Construct the graph comprising a single isolated vertex. An alias for the
-- | constructor 'Vertex'.
vertex :: forall a. a -> Graph a
vertex = Vertex

-- | Construct the graph comprising a single edge.
edge :: forall a. a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)

-- | Overlay two graphs. An alias for the constructor 'Overlay'. This is a
-- | commutative, associative and idempotent operation with the identity 'empty'.
overlay :: forall a. Graph a -> Graph a -> Graph a
overlay = Overlay

-- | Connect two graphs. An alias for the constructor 'Connect'. This is an
-- | associative operation with the identity 'empty', which distributes over
-- | 'overlay' and obeys the decomposition axiom.
connect :: forall a. Graph a -> Graph a -> Graph a
connect = Connect

-- | Construct the graph comprising a given array of isolated vertices.
vertices :: forall a. List a -> Graph a
vertices = overlays <<< map vertex

-- | Construct the graph from an array of edges.
edges :: forall a. List (Tuple a a) -> Graph a
edges = overlays <<< map (uncurry edge)

-- | Overlay a given list of graphs.
overlays :: forall a. List (Graph a) -> Graph a
overlays = fromMaybe empty <<< map (foldl1 overlay <<< NE.toNonEmpty) <<< NE.fromArray <<< toArray

-- | Connect a given list of graphs.
connects :: forall a. List (Graph a) -> Graph a
connects = fromMaybe empty <<< map (foldl1 connect <<< NE.toNonEmpty) <<< NE.fromArray <<< toArray

-- | Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
-- | the provided functions to the leaves and internal nodes of the expression.
-- | The order of arguments is: empty, vertex, overlay and connect.
foldg :: forall a b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldg e v o c = go
  where
  go = case _ of 
    Empty -> e
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
  Empty, Empty -> true
  Vertex x0, Vertex x1 -> x0 == x1
  Overlay x0 y0, Overlay x1 y1 -> x0 === x1 && y0 === y1
  Connect x0 y0, Connect x1 y1 -> x0 === x1 && y0 === y1
  _, _ -> false

infix 4 structuralEquality as ===

-- | Check if a graph is empty
-- |
-- | isEmpty empty                       == true
-- | isEmpty (overlay empty empty)       == true
-- | isEmpty (vertex x)                  == false
-- | isEmpty (removeVertex x $ vertex x) == true
-- | isEmpty (removeEdge x y $ edge x y) == false
isEmpty :: forall a. Graph a -> Boolean
isEmpty = foldg true (const false) (&&) (&&)

-- | The size of a graph, i.e. the number of leaves of the expression
-- | including empty leaves.
size :: forall a. Graph a -> Int
size = foldg 1 (const 1) (+) (+)

-- | Check if a graph contains a given vertex.
hasVertex :: forall a. Eq a => a -> Graph a -> Boolean
hasVertex x = foldg false (_ == x) (||) (||)

-- | Check if a graph contains a given edge.
hasEdge :: forall a. Eq a => a -> a -> Graph a -> Boolean
hasEdge s t g = hit g == Edge
  where
  hit Empty = Miss
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
vertexCount = Set.size <<< vertexSet

-- | The number of edges in a graph.
edgeCount :: forall a. Ord a => Graph a -> Int
edgeCount = AM.edgeCount <<< toAdjacencyMap

-- | The sorted list of vertices in a given graph 
vertexList :: forall a. Ord a => Graph a -> List a 
vertexList = fromArray <<< ISet.toAscArray <<< vertexSet

-- | The sorted list of edges of a graph.
edgeList :: forall a. Ord a => Graph a -> List (Tuple a a)
edgeList = AM.edgeList <<< toAdjacencyMap

-- | The set of vertices of a given graph.
vertexSet :: forall a. Ord a => Graph a -> Set a
vertexSet = foldg Set.empty Set.singleton Set.union Set.union

-- | The set of edges of a given graph.
edgeSet :: forall a. Ord a => Graph a -> Set (Tuple a a)
edgeSet = AM.edgeSet <<< toAdjacencyMap

-- | The sorted adjacency list of a graph.
adjacencyList :: forall a. Ord a => Graph a -> List (Tuple a (List a))
adjacencyList = AM.adjacencyList <<< toAdjacencyMap

-- | Convert a graph to 'AM.AdjacencyMap'.
toAdjacencyMap :: forall a. Ord a => Graph a -> AM.AdjacencyMap a
toAdjacencyMap = foldg AM.empty AM.vertex AM.overlay AM.connect

-- | The path on a list of vertices.
path :: forall a. List a -> Graph a
path xs = do 
  let xs' = toArray xs
  case Array.uncons xs' of
    Nothing -> empty
    Just { head, tail } 
      | Array.null tail -> vertex head
      | otherwise -> edges $ fromArray $ Array.zip xs' tail

-- | The circuit on a list of vertices.
circuit :: forall a. List a -> Graph a
circuit xs = do
  let xs' = toArray xs
  case Array.uncons xs' of
    Nothing -> empty
    Just { head, tail } -> path $ fromArray $ [ head ] <> tail <> [ head ]

-- | The clique on a list of vertices.
clique :: forall a. List a -> Graph a
clique = connects <<< map vertex

-- | The biclique on two lists of vertices.
biclique :: forall a. List a -> List a -> Graph a
biclique xs ys 
  | Array.null (toArray ys) = vertices xs
  | Array.null (toArray xs) = vertices ys
  | otherwise = connect (vertices xs) (vertices ys) 

-- | The star formed by a centre vertex connected to a list of leaves.
star :: forall a. a -> List a -> Graph a
star x xs
  | Array.null (toArray xs) = vertex x
  | otherwise = connect (vertex x) (vertices xs)

-- | The stars formed by overlaying a list of stars. An inverse of
-- | adjacencyList.
stars :: forall a. List (Tuple a (List a)) -> Graph a
stars = overlays <<< map (uncurry star)

-- | TODO:
-- | mesh, torus, pairs, deBruijn

-- | Remove a vertex from a given graph.
removeVertex :: forall a. Eq a => a -> Graph a -> Graph a
removeVertex v = induce (_ /= v)

-- | Remove an edge from a given graph.
removeEdge :: forall a. Eq a => a -> a -> Graph a -> Graph a
removeEdge s t = filterContext s (_ /= s) (_ /= t)

-- | Filter vertices in a subgraph context.
filterContext :: forall a. Eq a => a -> (a -> Boolean) -> (a -> Boolean) -> Graph a -> Graph a
filterContext s i o g = maybe g go $ context (_ == s) g
  where
  go { inputs, outputs } = 
    induce (_ /= s) g 
      `overlay` transpose (star s (fromArray $ Array.filter i $ toArray inputs))
      `overlay` star s (fromArray $ Array.filter o $ toArray outputs)


-- | Replaces vertex x with vertex y in a given Graph. If y already exists, 
-- | x and y will be merged.
replaceVertex :: forall a. Eq a => a -> a -> Graph a -> Graph a
replaceVertex u v = map \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
mergeVertices :: forall a. (a -> Boolean) -> a -> Graph a -> Graph a
mergeVertices p v = map \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
splitVertex :: forall a. Eq a => a -> List a -> Graph a -> Graph a
splitVertex v us g = g >>= \w -> if w == v then vertices us else vertex w

-- | Transpose a given graph.
transpose :: forall a. Graph a -> Graph a
transpose = foldg Empty Vertex Overlay (flip Connect)

-- | Construct the induced subgraph of a given graph by removing the
-- | vertices that do not satisfy a given predicate.
induce :: forall a. (a -> Boolean) -> Graph a -> Graph a
induce p = foldg Empty (\x -> if p x then Vertex x else Empty) (k Overlay) (k Connect)
  where
  k _ x Empty = x -- Constant folding to get rid of Empty leaves
  k _ Empty y = y
  k f x y = f x y

-- | Simplify a graph expression. Semantically, this is the identity function,
-- but it simplifies a given expression according to the laws of the algebra.
-- The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
simplify :: forall a. Ord a => Graph a -> Graph a
simplify = foldg Empty Vertex (simple Overlay) (simple Connect)
  where
  simple :: forall g. Eq g => (g -> g -> g) -> g -> g -> g
  simple op x y = do 
    let z = op x y
    case unit of
      _ | x == z -> x
        | y == z -> y
        | otherwise -> z


-- | Left-to-right relational composition of graphs: vertices x and z are
-- | connected in the resulting graph if there is a vertex y, such that x is
-- | connected to y in the first graph, and y is connected to z in the
-- | second graph. There are no isolated vertices in the result. This operation is
-- | associative, has 'empty' and single-'vertex' graphs as /annihilating zeroes/,
-- | and distributes over 'overlay'.
compose :: forall a. Ord a => Graph a -> Graph a -> Graph a
compose x y = overlays $ fromArray do
  let
    mx = toAdjacencyMap (transpose x)
    my = toAdjacencyMap y
  v <- ISet.toAscArray $ Set.union (AM.vertexSet mx) (AM.vertexSet my)
  let
    xs = ISet.toAscArray (AM.postSet v mx)
    ys = ISet.toAscArray (AM.postSet v my)
  guard $ not (Array.null xs)
  guard $ not (Array.null ys)
  pure $ biclique (fromArray xs) (fromArray ys)

-- | Compute the Cartesian product of graphs.
box :: forall a b. Graph a -> Graph b -> Graph (Tuple a b)
box x y = overlay (fx <*> y) (fy <*> x)
  where
  fx = foldg empty (vertex <<< Tuple) overlay overlay x
  fy = foldg empty (vertex <<< flip Tuple) overlay overlay y

-- 'Focus' on a specified subgraph.
focus :: forall a. (a -> Boolean) -> Graph a -> Focus a
focus f = foldg emptyFocus (vertexFocus f) overlayFoci connectFoci

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