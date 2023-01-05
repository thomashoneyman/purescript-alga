module Main where

import Algebra.Graph as Graph
import Algebra.Graph.AdjacencyMap (vertices)
import Algebra.Graph.Internal (fromArray)
import Data.List.Types (List)
import Data.Function ((#))
import Data.Eq ((==))
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt, close)
import Prelude (Unit, bind, discard)



main :: Effect Unit
main = do
  inputInterface <- createConsoleInterface noCompletion
  setPrompt "> " inputInterface
  prompt inputInterface
  inputInterface # setLineHandler \s ->
    if s == "quit"
    then 
      close inputInterface
    else do
      let list = fromArray [1,2,3,4,5]      -- range    :: Int -> Int -> List Int
      let graph = vertices list -- vertices :: List a -> Graph a
      log s
      log "logged and loaded"
