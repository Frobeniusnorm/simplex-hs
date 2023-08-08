module Simplex4 where
import Debug.Trace (trace)
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import Data.List (minimumBy)
import Data.Function (on)
import Simplex
import Selection
simplex :: Matrix R -> Vector R -> Vector R -> SimplexResult
-- | Implementation of the basic version of the Simplex algorithm
-- Takes a minimization problem in standard form
simplex a b c = do
  -- last variables
  let basis = reverse [snd (size a) - i | i <- [1 .. fst (size a)]]
  let (l, u, p, _)  = lu a
  let invP = pinv p
  let invL = pinv l
  let invU = pinv u
  let xB = flatten $ pinv (a ?? (All, Pos (idxs basis))) <> asColumn b
  let x = assoc (snd $ size a) 0 (basis `zip` toList xB) :: Vector R

  simplexIt a x b c basis 0 [] invP invL invU
  where
    simplexIt a x b c basis it etas invP invL invU = do
      let aB = a ?? (All, Pos $ idxs basis)
      let n = [i | i <- [0 .. snd (size a) - 1], i `notElem` basis]
      let aN = a ?? (All, Pos $ idxs n)
      -- btran
      let colc =  (asColumn c :: Matrix R)
      -- basically aB y = cT, to calculate the reduced costs
      -- TODO: y^T = c_B^T E_k^-1 * ... * E_1^-1 * U^-1 * L^-1 * P^-1 
      let y = show x `trace` flatten $ tr (tr (colc ? basis) <> foldl (<>) (ident $ length basis) etas <> ((invU <> invL <> invP) ? basis))
      -- pricing (finds delta costs = reduced costs)
      let zn = flatten (colc ? n) - (tr aN #> y)
      -- check if already optimal
      if all (\z -> z + 1e-9 >= 0) (toList zn) then SimplexResult x basis it
      else do
        let (pricing, ratio) = dantzig
        -- simpler as it looks: we need the actual index, but only have zn, so we have to zip the indices in n with actual indices to index zn, but want the index in n
        let j = pricing a zn n
        -- ftran
        let w = flatten $ linearSolveLS aB (a ?? (All, Pos $ idxs [j]))
        -- ratio-test
        if all (\f -> f - 1e-9 <= 0) (toList w) then
          SimplexUnbounded
        else do
          let basisInd = [0 .. (fst (size a) - 1)]
          let xB = flatten (asColumn x ? basis)
          -- calculate all gammas (we need to check the ws which are already indexed to the basis, as well as xB) with their respective index relative to the basis
          let gammas = [( xB ! i / w ! i, i) | i <- basisInd, w ! i > 0]
          -- find minimum and its index in the basis
          let (gamma, il) = ratio a xB w basis
          -- update x and basis
          let xB' = xB - scale gamma w
          -- calculate eta
          let eil = assoc (size w) 0 [(il, 1)] :: Vector R
          let eta = ident (size w) - asColumn (scale (1.0 / w ! il) (w - eil)) <> asRow eil
          -- update index il (basis index) in basis with j (absolute index)
          let basis' = zipWith (\b k -> (if k == il then j else b)) basis basisInd
          let x' = assoc (size x) 0 ((j, gamma):(basis `zip` toList xB'))
          simplexIt a x' b c basis' (it + 1) (eta:etas) invP invL invU


