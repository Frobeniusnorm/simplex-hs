module Simplex1 where

import Debug.Trace (trace)
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

simplex :: Matrix R -> Vector R -> Vector R -> Maybe (Vector R)
-- | Implementation of the Simplex algorithm
-- Takes a minimization problem in standard form
simplex a b c = undefined

