module Algebra.Graph.NonEmpty.Focused where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Comonad (class Comonad, class Extend)
import Algebra.Graph.NonEmpty as NE

refocus :: forall a. Eq a => a -> FocusedGraph a -> Maybe (FocusedGraph a)
refocus a (FocusedGraph _ g) = case NE.focus (a == _) g of
  f | f.ok -> Just (FocusedGraph a g)
  _ -> Nothing

data FocusedGraph n
  = FocusedGraph n (NE.Graph n)
derive instance Ord a => Ord (FocusedGraph a)
derive instance (Eq a, Ord a) => Eq (FocusedGraph a)
derive instance Functor FocusedGraph

instance Extend FocusedGraph where
  extend f fg = map f (duplicate fg)
    where
      duplicate :: forall a. FocusedGraph a -> FocusedGraph (FocusedGraph a)
      duplicate fg'@(FocusedGraph _ g) = FocusedGraph fg' gg
        where
          gg = map (\a -> FocusedGraph a g) g

instance Comonad FocusedGraph where
  extract (FocusedGraph node _) = node
