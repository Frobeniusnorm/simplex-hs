module Helper where

import Numeric.LinearAlgebra
import Debug.Trace
import Simplex (ProblemType)

naturalToStandard :: Matrix R -> Vector R -> Vector R -> (Matrix R, Vector R, Vector R)
naturalToStandard a b c = do
  -- natural: max c^T x s.t. Ax <= b
  -- standard: min c^T x s.t. Ax = b, x >= 0
  -- for all inequality a^T x <= b constraints we introduce an y s.t. a^T x + y = b
  let na = a ||| ident (fst $ size a)
  (na, b, vjoin [-c, konst 0 (fst $ size a)])

standardToNatural :: Matrix R -> Vector R -> Vector R -> (Matrix R, Vector R, Vector R)
standardToNatural a b c = do
  -- standard: min c^T x s.t. Ax = b, x >= 0
  -- natural: max c^T x s.t. Ax <= b
  -- TODO optimitation: identify variables that occur just in one equation and where c is 0, those can be omitted by reversing naturalToStandard

  -- every a^T x = b becomes: a^T x <= b and a^T x <= -b, we need to ensure non negativity with the identity
  (fromBlocks [[a], [a], [-ident (snd $ size a)]], vjoin [b, -b, konst 0 (snd $ size a)], -c)

-- currently only lower bounds:
includeBounds :: ProblemType -> Matrix R -> Vector R -> Vector R -> Vector R -> (Matrix R, Vector R)
includeBounds t a b l u = do
  let (la, lb) = if size l > 0 then includeLowerBounds a b l else (a, b)
  if size u > 0 then includeHigherBounds la lb u else (la, lb)
  where
    includeLowerBounds :: Matrix R -> Vector R -> Vector R -> (Matrix R, Vector R)
    includeHigherBounds :: Matrix R -> Vector R -> Vector R -> (Matrix R, Vector R)
    includeLowerBounds a b l = (a, b - a #> l)
    includeHigherBounds a b u =
      -- (a  0) = b
      -- (1  1) = u
      trace (show (size a) ++ ", " ++ show (size u)) ((a ||| matrix (size u) (map (const 0) [0 .. size u * fst (size a) - 1])) 
        === (ident (snd $ size a) ||| ident (size u)), vjoin [b, u])


removeBounds :: ProblemType -> Matrix R -> Vector R -> Vector R -> Vector R -> Vector R -> Vector R
removeBounds t a b x l u =
  if size l > 0 then removeLowerBounds a b x l else x
  where
    removeLowerBounds :: Matrix R -> Vector R -> Vector R -> Vector R -> Vector R
    removeLowerBounds a b x l = x + vjoin [l, konst 0 (size x - size l)]
