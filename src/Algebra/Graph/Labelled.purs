module Algebra.Graph.Labelled where


data Graph e a
  = Vertex a
  | Connect e (Graph e a) (Graph e a)

-- ELGraph Bool a ~ Graph a

--  https://github.com/snowleopard/alga/issues/240

-- Does the semiring structure for Label (<.> one <+> zero)
-- give rise to the semiring structure of Graph?

-- data Graph e a
--   = Vertex a
--   | Connect zero (Graph e a) (Graph e a) --> Overlay
--   | Connect x (Graph e a) (Graph e a)

-- zero <+> x == x 
-- Connect (Overlay g h) (Overlay i j) == Connect ()
-- Connect (Overlay g h) (Connect i j) == 