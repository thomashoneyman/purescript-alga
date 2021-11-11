module Algebra.Graph.NonEmpty.Context where


-- type Context a = 
--   { inputs  :: List a
--   , vertex  :: a
--   , outputs :: List a 
--   }

-- data ContextGraph n 
--   = ContextGraph (Context n) (Graph n)

-- mkContext :: forall a. List a -> a -> List a -> Context a
-- mkContext inputs vertex outputs = { inputs, vertex, outputs }

-- context :: forall a. Eq a => Graph a -> a -> Maybe (Context a)
-- context g a = 
--   case focus (a == _) g of
--     f | f.ok -> case Array.head $ toArray f.vs of 
--       Just v -> Just { inputs: f.is, vertex: v, outputs: f.os }
--       Nothing -> Nothing
--     _ -> Nothing

-- derive instance Functor (ContextGraph)
-- -- map :: (a -> b) -> ContextGraph a -> ContextGraph b

-- -- decompose :: forall a. ContextGraph a -> Context' a -> Graph a
-- -- decompose 

-- refocus :: forall a. Eq a => a -> ContextGraph a -> Maybe (ContextGraph a)
-- refocus a (ContextGraph _ g) = case focus (a == _) g of
--   f | f.ok -> Just (ContextGraph ({vertex: a, inputs: f.is, outputs: f.os}) g)
--   _ -> Nothing

-- instance Extend (ContextGraph) where 
--   -- extend :: (w a -> b) -> w a -> w b
--   extend f fg = map f duplicate

-- duplicate :: forall a. ContextGraph a -> ContextGraph (ContextGraph a)
-- duplicate fg@(ContextGraph _ g) = ContextGraph fgs gg
--   where
--     -- foldg :: forall a b
--     --        . (a -> b)
--     --       -> (b -> b -> b) 
--     --       -> (b -> b -> b) 
--     --       -> Graph a 
--     --       -> b

--     -- focus :: forall a. (a -> Boolean) -> Graph a -> Focus a

--     -- gg = map 
--     --   (\a -> ContextGraph { inputs: fromArray []
--     --                       , outputs: fromArray []
--     --                       , vertex: a
--     --                       } g
--     --   ) g
--     -- 'Focus' on a specified subgraph.
--     focus' :: forall a. Graph a -> ContextGraph (ContextGraph a)
--     focus' = foldg vertexCG overlayCG connectCG

--     vertexCG :: forall a. a -> Context a
--     vertexCG a = mkContext mempty a mempty

--     overlayCG :: forall a. Context a -> Context a -> Context a
--     overlayCG a b = 

--     connectCG :: forall a. Context a -> Context a -> Context a
--     connectCG a = mkContext mempty a mempty

--     fgs = { inputs: fromArray []
--           , outputs: fromArray []
--           , vertex: fg
--           }

-- instance Eq a => Comonad ContextGraph where 
--   extract (ContextGraph {vertex} _) = vertex
