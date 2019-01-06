-- | Set functions not present in Data.Map, but which I needed for the 
-- | implementation. Should be opened as a PR on ordered-collections.
module Internal.Set where

import Prelude

import Data.Map as Map
import Data.Set (Set)
import Data.Tuple (Tuple(..), fst)
import Internal.Map (fromSet)

toAscArray :: forall a. Ord a => Set a -> Array a
toAscArray = map fst <<< Map.toUnfoldable <<< fromSet (\a -> Tuple a unit)