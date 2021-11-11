module Test.Main where

import Prelude
import Test.QuickCheck.Arbitrary

import Algebra.Graph (Graph, adjacencyList, circuit, connect, context, edge, empty, overlay, path, simplify, size, toAdjacencyMap, vertex, transpose)
import Algebra.Graph as Graph
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap.Undirected as AMU
import Algebra.Graph.Internal (fromArray)
import Algebra.Graph.Undirected as UG
import Control.Comonad (duplicate, extend, extract)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Test.QuickCheck (mkSeed, (<?>), (===))
import Test.QuickCheck.Gen (Gen, runGen, resize)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "AdjacencyMap" do
      it "makes an empty unit graph" do
        AM.adjacencyList (AM.empty :: AdjacencyMap Unit) 
          `shouldEqual` fromArray []
      it "builds a simple cycle" do
        unwrap (AM.connect (AM.vertex true) (AM.vertex false) `AM.overlay` AM.connect (AM.vertex false) (AM.vertex true))
          `shouldEqual` Map.fromFoldable [ Tuple false (Set.singleton true), Tuple true (Set.singleton false) ]
      it "connects simple AMs" do
        AM.adjacencyList (AM.connect (AM.vertex true) (AM.vertex false))
          `shouldEqual` fromArray [ Tuple false (fromArray []), Tuple true (fromArray [ false ]) ]
      it "connects another simple AM" do
        AM.adjacencyList (AM.connect (AM.edge true false) (AM.vertex false))
          `shouldEqual` fromArray [ Tuple false (fromArray [ false ]), Tuple true (fromArray [ false ]) ]
    describe "AdjacencyMap.Undirected" do
      it "connects simple AMs" do
        AMU.adjacencyList (AMU.connect (AMU.vertex true) (AMU.vertex false))
          `shouldEqual` fromArray [ Tuple false (fromArray [ true ]), Tuple true (fromArray [ false ]) ]
      it "connects another simple AM" do
        AMU.adjacencyList (AMU.connect (AMU.edge true false) (AMU.vertex false))
          `shouldEqual` fromArray [ Tuple false (fromArray [ false, true ]), Tuple true (fromArray [ false ]) ]
    describe "Graph Unit" do
      it "makes an empty unit graph" do
        adjacencyList (empty :: Graph Unit) 
          `shouldEqual` fromArray []
      it "makes a singleton unit graph" do
        adjacencyList (vertex unit) 
          `shouldEqual` fromArray [Tuple unit (fromArray [])]
      it "overlays the same singleton unit graph" do
        adjacencyList (overlay (vertex unit) (vertex unit))
          `shouldEqual` fromArray [Tuple unit (fromArray [])]
      it "an edge in the unit graph" do
        adjacencyList (edge unit unit) 
          `shouldEqual` fromArray [Tuple unit (fromArray [unit])]
      it "connects the same singleton graph" do
        adjacencyList (connect (vertex unit) (vertex unit))
          `shouldEqual` fromArray [Tuple unit (fromArray [unit])]
    describe "Graph Boolean" do
      it "builds a simple cycle" do
        adjacencyList (connect (vertex true) (vertex false) `overlay` connect (vertex false) (vertex true))
          `shouldEqual` fromArray [ Tuple false (fromArray [ true ]), Tuple true (fromArray [ false ]) ]
      it "adjacencyMap of simple boolean graph" do
        show (AM.adjacencyList $ toAdjacencyMap ((edge true false) `connect` (vertex false)))
          `shouldEqual` "[(Tuple false [false]),(Tuple true [false])]"
      it "tranpose of the transpose is identity for a simple boolean graph" do
        quickCheck \(g :: Graph Boolean) -> g == (transpose $ transpose g) <?> ("g: " <> show g <> " / toAdjacencyMap g : " <> show (toAdjacencyMap g) <> " / AM.symmetricClosure $ toAdjacencyMap g: " <> (show $ AM.symmetricClosure $ toAdjacencyMap g))
    describe "Graph Char" do
      it "makes an empty char graph" do
        adjacencyList (empty :: Graph Char) 
          `shouldEqual` fromArray []
      it "makes a singleton char graph" do
        adjacencyList (vertex 'a') 
          `shouldEqual` fromArray [Tuple 'a' (fromArray [])]
      it "makes a directed version of the example graph from comonad of graph decompositions" do
        adjacencyList (circuit (fromArray [ 'a', 'b', 'f' ]) `overlay` path (fromArray [ 'b', 'c', 'd', 'e' ]))
          `shouldEqual` fromArray [ Tuple 'a' (fromArray [ 'b' ]), Tuple 'b' (fromArray [ 'c', 'f' ]), Tuple 'c' (fromArray [ 'd' ]), Tuple 'd' (fromArray [ 'e' ]), Tuple 'e' (fromArray []), Tuple 'f' (fromArray [ 'a' ]) ]
    describe "Equalities" do
      it "is structurally equal implies is equal" do
        -- manually coding logical implication (p => q) as (if p then q else true)
        quickCheck \(g :: Graph Int) (h :: Graph Int) -> (if (g Graph.=== h) then (g == h) else true) <?> ((show (g Graph.=== h)) <> " ≠ " <> show (g == h) <> " / g: " <> show g <> " h: " <> show h)
    describe "Simplifying Graphs" do
      it "simplifies an empty graph" do
        simplify (empty :: Graph Unit)
          `shouldEqual` empty
      it "simplifies a redundent self-edge" do
        simplify (connect (edge 'a' 'a') (vertex 'a'))
          `shouldEqual` (edge 'a' 'a')
      it "simplifies a more redundent self-edge" do
        simplify (connect (vertex 'a') (connect (edge 'a' 'a') (vertex 'a')))
          `shouldEqual` (edge 'a' 'a')
      it "simplifies arbitrary graphs" do
        quickCheck \(g :: Graph Int) -> (overlay g g) === g
    describe "contexts of Graphs" do
      it "works with empty graph" do
        context (unit == _) (empty :: Graph Unit)
          `shouldEqual` Nothing
      it "works with false predicate" do
        context (const false) (empty :: Graph Unit)
          `shouldEqual` Nothing
      it "works with a simple context" do
        context (true == _) (edge true false)
          `shouldEqual` (Just ({ inputs: fromArray [], outputs: fromArray [ false ] }))
      it "works with the maximal subgraph" do
        context (const true) (edge true false)
          `shouldEqual` (Just ({ inputs: fromArray [ true ], outputs: fromArray [ false ] }))
      it "works with an example graph" do
        context (4 == _)
          ( (vertex 3)
              `connect`
                (vertex 1)
              `connect`
                (vertex 4)
              `connect`
                (vertex 1)
              `connect`
                (vertex 5)
          )
          `shouldEqual` (Just ({ inputs: fromArray [ 3, 1 ], outputs: fromArray [ 1, 5 ] }))
    describe "UndirectedGraph Boolean" do
      it "adjacencyMap of simple boolean graph" do
        show (AMU.adjacencyList $ UG.toAdjacencyMap ((UG.edge true false) `UG.connect` (UG.vertex false)))
          `shouldEqual` "[(Tuple false [false,true]),(Tuple true [false])]"
      it "remove edges from a simple boolean graph" do
        show (AMU.adjacencyList $ UG.toAdjacencyMap $ UG.removeEdge true false $ UG.edge true false `UG.connect` (UG.vertex false))
          `shouldEqual` "[(Tuple false [false]),(Tuple true [])]"
      it "remove edges from a simple boolean graph" do
        show (AMU.adjacencyList $ UG.toAdjacencyMap $ UG.removeEdge false true $ UG.edge true false `UG.connect` (UG.vertex false))
          `shouldEqual` "[(Tuple false [false]),(Tuple true [])]"
      it "remove edges from a simple boolean graph" do
        show (AMU.adjacencyList $ UG.toAdjacencyMap $ UG.removeEdge false false $ UG.edge true false `UG.connect` (UG.vertex false))
          `shouldEqual` "[(Tuple false [true]),(Tuple true [false])]"
      it "counts edges for a simple boolean graph" do
        UG.edgeCount (UG.edge true false `UG.connect` (UG.vertex false))
          `shouldEqual` 2
      it "adjacencyMap of simple boolean graph" do
        show (AMU.adjacencyList $ UG.toAdjacencyMap $ UG.simplify ((UG.edge true false) `UG.connect` (UG.vertex false)))
          `shouldEqual` "[(Tuple false [false,true]),(Tuple true [false])]"
      it "counts edges for a simple boolean graph" do
        UG.edgeCount (UG.simplify $ UG.edge true false `UG.connect` (UG.vertex false))
          `shouldEqual` 2
      it "is a symmetric closure of itself for a simple boolean graph" do
        quickCheck \(g :: UG.Graph Boolean) -> UG.toAdjacencyMap g == (AMU.symmetricClosure $ UG.toAdjacencyMap g) <?> ("g: " <> show g)
    describe "UndirectedGraph Char" do
      it "make a simple undirected edge" do
        UG.adjacencyList (UG.edge true false)
          `shouldEqual` fromArray [ Tuple false (fromArray [ true ]), Tuple true (fromArray [ false ]) ]
      it "make a simple reflexive undirected edge" do
        UG.adjacencyList ((UG.edge true false) `UG.overlay` (UG.edge true true) `UG.overlay` (UG.edge false false))
          `shouldEqual` fromArray [ Tuple false (fromArray [ false, true ]), Tuple true (fromArray [ false, true ]) ]
      it "makes the example graph from comonad of graph decompositions" do
        UG.adjacencyList
          ( ( UG.circuit (fromArray [ 'a', 'b', 'f' ])
                `UG.overlay` UG.path (fromArray [ 'b', 'c', 'd', 'e' ])
            )
              `UG.overlay` UG.edge 'f' 'd'
          )
          `shouldEqual` fromArray [ Tuple 'a' (fromArray [ 'b', 'f' ]), Tuple 'b' (fromArray [ 'a', 'c', 'f' ]), Tuple 'c' (fromArray [ 'b', 'd' ]), Tuple 'd' (fromArray [ 'c', 'e', 'f' ]), Tuple 'e' (fromArray [ 'd' ]), Tuple 'f' (fromArray [ 'a', 'b', 'd' ]) ]
    describe "Extend Graph" do
      it "extends const identity to the same graph" do
        extend (size) (vertex unit)
          `shouldEqual` vertex 1
      it "extends const 'a' to one node self edge graph" do
        (simplify $ extend (const 'a') (circuit (fromArray [ 'a', 'b', 'f' ]) `overlay` path (fromArray [ 'b', 'c', 'd', 'e' ])))
          `shouldEqual` (edge 'a' 'a')
      it "extends identity to a graph of graphs" do
        (simplify $ extend identity (edge 'a' 'a'))
          `shouldEqual` (edge (edge 'a' 'a') (edge 'a' 'a'))
