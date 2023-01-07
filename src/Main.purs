module Main where

import Algebra.Graph (Graph(..), edge, overlay)
import Algebra.Graph.AdjacencyMap (vertices, clique)
import Algebra.Graph.Internal (fromArray)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (pure)
import Control.Monad.State (State)
import Data.Eq ((==))
import Data.Function ((#))
import Data.Functor (class Functor)
import Data.List.Types (List(..), (:))
import Data.Ord ((>=))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Random (randomInt)
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt, close)
import Prelude (class Applicative, Unit, bind, discard)
import Test.Main (boolList)

main :: Effect Unit
main = do
  item <- boolList 5 5 (1:2:3:4:5:Nil)
  logShow item
  log "compiled"