-- | This module defines various internal utilities and data structures used
-- | throughout the library, such as lists with fast concatenation. The API
-- | is unstable and unsafe, and is exposed only for documentation.
module Algebra.Graph.Internal where

import Prelude

import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class Alt, class Alternative, class MonadZero, class Plus, alt)
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr, unfoldr1)
import Internal.Set as ISet

-- | An abstract list data type with /O(1)/ time concatenation (the current
-- | implementation uses difference lists). Here @a@ is the type of list elements.
-- | 'List' @a@ is a 'Monoid': 'mempty' corresponds to the empty list and two lists
-- | can be concatenated with 'mappend' (or operator 'Data.Monoid.<>'). Singleton
-- | lists can be constructed using the function 'pure' from the 'Applicative'
-- | instance. 'List' @a@ is also an instance of 'IsList', therefore you can use
-- | list literals, e.g. @[1,4]@ @::@ 'List' @Int@ is the same as 'pure' @1@
-- | 'Data.Monoid.<>' 'pure' @4@; note that this requires the @OverloadedLists@
-- | GHC extension. To extract plain Haskell lists you can use the 'toList'
-- | function from the 'Foldable' instance.
newtype List a = List (Endo (->) (Array a))

derive newtype instance semigroupList :: Semigroup (List a)
derive newtype instance monoidList :: Monoid (List a)

instance showList :: Show a => Show (List a) where
  show = show <<< toArray

instance eqList :: Eq a => Eq (List a) where
  eq x y = toArray x == toArray y

instance ordList :: Ord a => Ord (List a) where
  compare x y = compare (toArray x) (toArray y)

instance unfoldable1List :: Unfoldable1 List where
  unfoldr1 f b = fromArray (unfoldr1 f b)

instance unfoldableList :: Unfoldable List where
  unfoldr f b = fromArray (unfoldr f b)

instance foldableList :: Foldable List where
  foldr f b = foldr f b <<< toArray
  foldl f b = foldl f b <<< toArray
  foldMap f = foldMap f <<< toArray

instance functorList :: Functor List where 
  map f = fromArray <<< map f <<< toArray

instance applyList :: Apply List where
  apply f x = fromArray (toArray f <*> toArray x)

instance applicativeList :: Applicative List where
  pure = List <<< Endo <<< (:)

instance bindList :: Bind List where
  bind x f = fromArray (toArray x >>= (toArray <<< f))

instance monadList :: Monad List

instance plusList :: Plus List where
  empty = mempty

instance altList :: Alt List where
  alt a b = fromArray (alt (toArray a) (toArray b))

instance alternativeList :: Alternative List

instance monadPlusList  :: MonadPlus List

instance monadZeroList :: MonadZero List

toArray :: List ~> Array
toArray (List (Endo f)) = f []

toUnfoldable :: forall f. Unfoldable f => List ~> f
toUnfoldable (List (Endo f)) = Array.toUnfoldable $ f []

fromArray :: Array ~> List
fromArray = List <<< Endo <<< append

fromFoldable :: forall f. Foldable f => f ~> List
fromFoldable = List <<< Endo <<< append <<< Array.fromFoldable

-- | The focus of a graph expression is a flattened represenentation of the
-- | subgraph under focus, its context, as well as the list of all encountered
-- | vertices. See 'Algebra.Graph.removeEdge' for a use-case example.
type Focus a = 
  { ok :: Boolean -- true if focus on the specified subgraph is obtained
  , is :: List a  -- inputs into the focused subgraph
  , os :: List a  -- outputs out of the focused subgraph
  , vs :: List a  -- all vertices (leaves) of the graph expression
  }

mkFocus :: forall a. Boolean -> List a -> List a -> List a -> Focus a
mkFocus a b c d = { ok: a, is: b, os: c, vs: d }

-- | Focus on the empty graph.
emptyFocus :: forall a. Focus a
emptyFocus = mkFocus false mempty mempty mempty 

-- | Focus on the graph with a single vertex, given a predicate indicating
-- | whether the vertex is of interest.
vertexFocus :: forall a. (a -> Boolean) -> a -> Focus a
vertexFocus f x = mkFocus (f x) mempty mempty (pure x)

-- | Overlay two foci
overlayFoci :: forall a. Focus a -> Focus a -> Focus a
overlayFoci x y = mkFocus (x.ok || y.ok) (x.is <> y.is) (x.os <> y.os) (x.vs <> y.vs)

-- | Connect two foci.
connectFoci :: forall a. Focus a -> Focus a -> Focus a
connectFoci x y = mkFocus (x.ok || y.ok) (xs <> y.is) (x.os <> ys) (x.vs <> y.vs)
  where
    xs = if y.ok then x.vs else x.is
    ys = if x.ok then y.vs else y.os

-- | An auxiliary data type for 'hasEdge': when searching for an edge, we can hit
-- | its 'Tail', i.e. the source vertex, the whole 'Edge', or 'Miss' it entirely.
data Hit 
  = Miss 
  | Tail 
  | Edge

derive instance eqHit :: Eq Hit
derive instance ordHit :: Ord Hit

-- | A safe version of 'foldr1'.
foldr1Safe :: forall a. (a -> a -> a) -> List a -> Maybe a
foldr1Safe f = foldr (maybeF f) Nothing

-- | Auxiliary function that tries to apply a function to a base case and a 'Maybe'
-- | value and return 'Just' the result or 'Just' the base case.
maybeF :: forall a b. (a -> b -> a) -> a -> Maybe b -> Maybe a
maybeF f x = Just <<< maybe x (f x)

-- | Compute the Cartesian product of two sets.
setProduct :: forall a b. Ord a => Ord b => Set a -> Set b -> Set (Tuple a b)
setProduct = setProductWith Tuple

-- | Compute the Cartesian product of two sets, applying a function to each
-- | resulting pair.
setProductWith :: forall a b c. Ord a => Ord b => Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
setProductWith f x y = Set.fromFoldable do 
  a <- ISet.toAscArray x
  b <- ISet.toAscArray y
  pure $ f a b