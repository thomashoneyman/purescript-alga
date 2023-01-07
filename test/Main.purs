module Test.Main where

import Algebra.Graph (Graph(..), edge, overlay)
import Algebra.Graph.AdjacencyMap (vertices, clique)
import Algebra.Graph.Internal (fromArray)
import Control.Monad (pure)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad.State (State)
import Data.Eq ((==))
import Data.Function ((#))
import Data.Functor (class Functor)
import Data.List.Types (List(..), (:))
import Data.Ord ((>=))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt, close)
import Prelude (class Applicative, Unit, bind, discard)

-- state impl for now
newtype Stat3 s v = Stat3 (s -> Tuple v s)

runStat3 :: forall s v. Stat3 s v -> s -> Tuple v s
runStat3 (Stat3 g) s = g s

instance functorStat3 :: Functor (Stat3 s) where
--map :: forall a b. (a -> b) -> f a -> f b
  map g f = Stat3 (\s -> let Tuple v s' = runStat3 f s in Tuple (g v) s')

instance applyStat3 :: Functor (Stat3 s) => Apply (Stat3 s) where
--apply :: forall a b. f (a -> b) -> f a -> f b
  apply fg f = Stat3 (\s -> let Tuple g s'  = runStat3 fg s
                                Tuple v s'' = runStat3 f s' in Tuple (g v) s'')

instance applicativeStat3 :: Apply (Stat3 s) => Applicative (Stat3 s) where
--pure :: forall a. a -> f a
  pure v = Stat3 (\s -> Tuple v s)

instance bindStat3 :: Applicative (Stat3 s) => Bind (Stat3 s) where
  bind m g = Stat3 (\s -> let Tuple v s' = runStat3 m s in runStat3 (g v) s')

-- Add edge within state
add3dge :: forall a. a -> a -> Stat3 (Graph a) (Graph a)
add3dge s t = Stat3 (\g -> let e = edge s t in Tuple e (overlay g e))

addNod3 :: forall a. a -> Stat3 (Graph a) (Graph a)
addNod3 n = Stat3 (\g -> let newV = Vertex n in Tuple newV (overlay g newV))

compareLists :: List Int -> List Int -> List Boolean
compareLists (x:xs) (y:ys) = 
  Cons (x >= y) (compareLists xs ys)
compareLists _ (Cons _ _) = Nil
compareLists _ _ = Nil

boolList :: Int -> Int -> List Int -> Effect (List Boolean)
boolList numNodes maxNum edgeCounts = do
  randoms :: List Int <- replicateA numNodes (randomInt 1 maxNum)
  pure (compareLists randoms edgeCounts)
  
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
      let list = fromArray [1,2,3,4,5] -- range    :: Int -> Int -> List Int
      let initGraph = clique list       -- vertices :: List a -> Graph a
      log s
      log "logged and loaded"
