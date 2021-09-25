module Test.Main where

import Prelude

import Algebra.Graph (Graph(..), adjacencyList, circuit, connect, edge, empty, overlay, path, vertex)
import Algebra.Graph.AdjacencyMap (AdjacencyMap(..))
import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.Undirected as UG
import Algebra.Graph.Internal (fromArray)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (logShow)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
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
          `shouldEqual` Map.fromFoldable [Tuple false (Set.singleton true), Tuple true (Set.singleton false)]
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
          `shouldEqual` fromArray [Tuple false (fromArray [true]), Tuple true (fromArray [false])]
    describe "Graph Char" do
      it "makes an empty char graph" do
        adjacencyList (empty :: Graph Char) 
          `shouldEqual` fromArray []
      it "makes a singleton char graph" do
        adjacencyList (vertex 'a') 
          `shouldEqual` fromArray [Tuple 'a' (fromArray [])]
      it "makes a directed version of the example graph from comonad of graph decompositions" do
        adjacencyList (circuit (fromArray ['a', 'b', 'f']) `overlay` path (fromArray ['b', 'c', 'd', 'e']))
          `shouldEqual` fromArray [Tuple 'a' (fromArray ['b']), Tuple 'b' (fromArray ['c', 'f']), Tuple 'c' (fromArray ['d']), Tuple 'd' (fromArray ['e']), Tuple 'e' (fromArray []), Tuple 'f' (fromArray ['a'])]
    describe "UndirectedGraph Char" do
      it "makes the example graph from comonad of graph decompositions" do
        UG.adjacencyList (UG.circuit (fromArray ['a', 'b', 'f']) `UG.overlay` UG.path (fromArray ['b', 'c', 'd', 'e']))
          `shouldEqual` fromArray [Tuple 'a' (fromArray ['b', 'f']), Tuple 'b' (fromArray ['c', 'f']), Tuple 'c' (fromArray ['d']), Tuple 'd' (fromArray ['e']), Tuple 'e' (fromArray ['d']), Tuple 'f' (fromArray ['a', 'd'])]
