-- | This module defines the 'AdjacencyMap' data type and associated functions.
-- | 
-- | Missing members:
-- |   - tree
-- |   - forest
module Algebra.Graph.NonEmpty.AdjacencyMap 
  ( -- Data structure
    module Algebra.Graph.AdjacencyMap.Internal,
    -- Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,
    -- Relations on graphs
    isSubgraphOf,
    -- Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, edgeSet, preSet, postSet,
    -- Standard families of graphs
    path, circuit, clique, biclique, star, stars, fromAdjacencySets, -- missing tree, forest
    -- Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, transpose, gmap, induce,
    -- Relational operations
    compose, closure, reflexiveClosure, symmetricClosure, transitiveClosure
  ) where

import Prelude

import Algebra.Graph.AdjacencyMap.Internal (AdjacencyMap(..))
import Algebra.Graph.Internal (List, fromArray, toArray)
import Control.Fold as Fold
import Control.MonadZero (guard)
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Internal.Map as IMap
import Internal.Set as ISet

-- | Construct the empty graph
empty :: forall a. AdjacencyMap a
empty = AM Map.empty

-- | Construct the graph comprising a single isolated vertex.
vertex :: forall a. a -> AdjacencyMap a
vertex x = AM $ Map.singleton x Set.empty

-- | Construct the graph comprising a single edge.
edge :: forall a. Ord a => a -> a -> AdjacencyMap a
edge x y
  | x == y = AM $ Map.singleton x (Set.singleton y)
  | otherwise = AM $ Map.fromFoldable [ Tuple x (Set.singleton y), Tuple y Set.empty ]

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- | operation with the identity 'empty'.
overlay :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay = add

-- | /Connect/ two graphs. This is an associative operation with the identity
-- | 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
connect :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect = mul

-- | Construct the graph comprising a given list of isolated vertices.
vertices :: forall a. Ord a => List a -> AdjacencyMap a
vertices = AM <<< Map.fromFoldable <<< map (\x -> Tuple x Set.empty)

-- | Construct the graph from a list of edges.
edges :: forall a. Ord a => List (Tuple a a) -> AdjacencyMap a
edges = fromAdjacencySets <<< map (map Set.singleton)

-- | Overlay a given list of graphs.
overlays :: forall a. Ord a => List (AdjacencyMap a) -> AdjacencyMap a
overlays = AM <<< IMap.unionsWith Set.union <<< map unwrap

-- | Connect a given list of graphs.
connects :: forall a. Ord a => List (AdjacencyMap a) -> AdjacencyMap a
connects = foldr connect empty

-- | Returns 'True' if the first graph is a subgraph of the second.
isSubgraphOf :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> Boolean
isSubgraphOf (AM x) (AM y) = IMap.isSubmapBy Set.subset x y

-- | Check if a graph is empty.
isEmpty :: forall a. AdjacencyMap a -> Boolean
isEmpty = Map.isEmpty <<< unwrap

-- | Check if a graph contains a given vertex.
hasVertex :: forall a. Ord a => a -> AdjacencyMap a -> Boolean
hasVertex v = Map.member v <<< unwrap

-- | Check if a graph contains a given edge.
hasEdge :: forall a. Ord a => a -> a -> AdjacencyMap a -> Boolean
hasEdge v0 v1 (AM m) = case Map.lookup v0 m of
  Nothing -> false
  Just vs -> Set.member v1 vs

-- | The number of vertices in a graph.
vertexCount :: forall a. AdjacencyMap a -> Int
vertexCount = Map.size <<< unwrap

-- | The number of edges in a graph.
edgeCount :: forall a. AdjacencyMap a -> Int 
edgeCount = Fold.foldl Fold.sum <<< map Set.size <<< unwrap

-- | The sorted list of vertices of a given graph.
vertexList :: forall a. AdjacencyMap a -> List a
vertexList = map fst <<< Map.toUnfoldable <<< unwrap

-- | The sorted list of edges of a graph.
edgeList :: forall a. Ord a => AdjacencyMap a -> List (Tuple a a)
edgeList (AM m) = do 
  Tuple x ys <- Map.toUnfoldable m 
  y <- fromArray $ ISet.toAscArray ys
  pure $ Tuple x y

-- | The set of vertices of a given graph.
edgeSet :: forall a. Ord a => AdjacencyMap a -> Set (Tuple a a)
edgeSet = Set.fromFoldable <<< edgeList

-- | The set of vertices of a given graph.
vertexSet :: forall a. AdjacencyMap a -> Set a
vertexSet = Map.keys <<< unwrap

-- | The sorted adjacency list of a graph.
adjacencyList :: forall a. Ord a => AdjacencyMap a -> List (Tuple a (List a))
adjacencyList = map (map (fromArray <<< ISet.toAscArray)) <<< Map.toUnfoldable <<< unwrap

-- | The preset of an element x is the set of its direct predecessors.
preSet :: forall a. Ord a => a -> AdjacencyMap a -> Set a
preSet x = Set.fromFoldable <<< map fst <<< Array.filter p <<< Map.toUnfoldable <<< unwrap
  where
  p (Tuple _ set) = Set.member x set

-- | The postset of a vertex is the set of its direct successors.
postSet :: forall a. Ord a => a -> AdjacencyMap a -> Set a
postSet x = fromMaybe Set.empty <<< Map.lookup x <<< unwrap

-- | The path on a list of vertices.
path :: forall a. Ord a => List a -> AdjacencyMap a
path xs = do 
  let xs' = toArray xs 
  case Array.uncons xs' of 
    Nothing -> empty
    Just { head, tail } 
      | Array.null tail -> vertex head
      | otherwise -> edges (fromArray (Array.zip xs' tail))

-- | The circuit on a list of vertices.
circuit :: forall a. Ord a => List a -> AdjacencyMap a
circuit xs = case Array.uncons (toArray xs) of
  Nothing -> empty
  Just { head, tail } -> path $ fromArray $ [head] <> tail <> [head]

-- | The clique on a list of vertices.
clique :: forall a. Ord a => List a -> AdjacencyMap a
clique = fromAdjacencySets <<< fromArray <<< fst <<< go <<< toArray
  where
  go xs = case Array.uncons xs of
    Nothing -> Tuple [] Set.empty
    Just { head, tail } -> do
      let Tuple res set = go tail 
      Tuple ((Tuple head set) : res) (Set.insert head set)

-- | The biclique on two lists of vertices.
biclique :: forall a. Ord a => List a -> List a -> AdjacencyMap a
biclique xs ys = AM $ IMap.fromSet adjacent (Set.union x y)
  where
  x = Set.fromFoldable xs
  y = Set.fromFoldable ys
  adjacent v = if Set.member v x then y else Set.empty

-- | The star formed by a centre vertex connected to a list of leaves.
star :: forall a. Ord a => a -> List a -> AdjacencyMap a
star x ys 
  | ys == mempty = vertex x
  | otherwise = connect (vertex x) (vertices ys)

-- | The stars formed by overlaying a list of 'star's. An inverse of
-- | 'adjacencyList'.
stars :: forall a. Ord a => List (Tuple a (List a)) -> AdjacencyMap a
stars = fromAdjacencySets <<< map (map Set.fromFoldable)

-- | Construct a graph from a list of adjacency sets; a variation of 'stars'.
fromAdjacencySets :: forall a. Ord a => List (Tuple a (Set a)) -> AdjacencyMap a
fromAdjacencySets ss = AM $ Map.unionWith Set.union vs es
  where
  vs = IMap.fromSet (const Set.empty) $ Set.unions $ map snd ss
  es = Map.fromFoldableWith Set.union ss

-- | Remove a vertex from a given graph.
removeVertex :: forall a. Ord a => a -> AdjacencyMap a -> AdjacencyMap a
removeVertex x = AM <<< map (Set.delete x) <<< Map.delete x <<< unwrap

-- | Remove an edge from a given graph.
removeEdge :: forall a. Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge x y = AM <<< Map.update (Just <<< Set.delete y) x <<< unwrap

-- | Replaces vertex x with vertex y in a given 'AdjacencyMap'. If y already 
-- | exists, x and y will be merged.
replaceVertex :: forall a. Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
replaceVertex u v = gmap \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
mergeVertices :: forall a. Ord a => (a -> Boolean) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices p v = gmap \u -> if p u then v else u

-- | Transpose a given graph.
transpose :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
transpose (AM m) = AM $ foldrWithIndex combine vs m
  where
  combine v es = Map.unionWith Set.union (IMap.fromSet (const $ Set.singleton v) es)
  vs = IMap.fromSet (const Set.empty) (Map.keys m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- | similar to Functor's 'map' but can be used with non-fully-parametric adjacency maps
gmap :: forall a b. Ord a => Ord b => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = AM <<< map (Set.map f) <<< IMap.mapKeysWith Set.union f <<< unwrap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- | vertices that do not satisfy a given predicate.
induce :: forall a. Ord a => (a -> Boolean) -> AdjacencyMap a -> AdjacencyMap a
induce p = AM <<< map (Set.filter p) <<< Map.filterWithKey (\k _ -> p k) <<< unwrap

-- | Left-to-right relational composition of graphs: vertices x and z are
-- | connected in the resulting graph if there is a vertex y, such that x is
-- | connected to y in the first graph, and y is connected to z in the second 
-- | graph. There are no isolated vertices in the result. This operation is
-- | associative, has 'empty' and single-'vertex' graphs as annihilating zeroes,
-- | and distributes over 'overlay'.
compose :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
compose x y = fromAdjacencySets do
  let vs = Set.union (vertexSet x) (vertexSet y)
  v <- Set.toUnfoldable vs
  let ys = postSet v y
  guard $ not $ Set.isEmpty ys
  t <- Set.toUnfoldable (postSet v (transpose x))
  pure $ Tuple t ys

-- | Compute the reflexive and transitive closure of a graph.
closure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
closure = reflexiveClosure <<< transitiveClosure

-- | Compute the reflexive closure of a graph by adding a self-loop to every vertex.
reflexiveClosure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
reflexiveClosure = AM <<< mapWithIndex Set.insert <<< unwrap

-- | Compute the symmetric closure of a graph by overlaying it with its own transpose.
symmetricClosure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
symmetricClosure m = overlay m (transpose m)

-- | Compute the transitive closure of a graph.
transitiveClosure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure old = do
  let new = overlay old (old `compose` old)
  if old == new then old else transitiveClosure new