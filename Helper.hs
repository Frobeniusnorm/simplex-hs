module Helper where

import Numeric.LinearAlgebra

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

