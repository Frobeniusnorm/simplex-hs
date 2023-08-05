module Simplex3 where
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import Data.List (minimumBy)
import Data.Function (on)
simplex :: Matrix R -> Vector R -> Vector R -> Maybe (Vector R)
-- | Modular Implementation of the basic version of the Simplex algorithm
-- Takes a minimization problem in standard form
-- Customizable pricing methods
simplex a b c = do
  -- last variables
  let basis = reverse [snd (size a) - i | i <- [1 .. fst (size a)]]
  let xB = flatten $ pinv (a ?? (All, Pos (idxs basis))) <> asColumn b
  let x = assoc (snd $ size a) 0 (basis `zip` toList xB) :: Vector R
  simplexIt a x b c basis
  where
    simplexIt a x b c basis = do
      let aB = a ?? (All, Pos $ idxs basis)
      let n = [i | i <- [0 .. snd (size a) - 1], i `notElem` basis]
      let aN = a ?? (All, Pos $ idxs n)
      -- btran
      let colc = asColumn c :: Matrix R
      -- basically aB y = cT, to calculate the reduced costs
      let y = flatten $ linearSolveLS (tr aB) (colc ? basis)
      -- pricing (finds delta costs = reduced costs)
      let zn = flatten (colc ? n) - (tr aN #> y)
      -- check if already optimal
      if all (\z -> z + 1e-9 >= 0) (toList zn) then do
        Just x
      else do
        -- ftran
        -- simpler as it looks: we need the actual index, but only have zn, so we have to zip the indices in n with actual indices to index zn, but want the index in n
        let j = snd $ head $ filter (\zj -> fst zj < 0) (zipWith (\i j -> (zn ! i, j)) [0 .. (snd (size a) - fst (size a) - 1)] n)
        let w = flatten $ linearSolveLS aB (a ?? (All, Pos $ idxs [j]))
        -- ratio-test
        if all (\f -> f - 1e-9 <= 0) (toList w) then
          Nothing
        else do
          let basisInd = [0 .. (fst (size a) - 1)]
          let xB = flatten (asColumn x ? basis)
          -- calculate all gammas (we need to check the ws which are already indexed to the basis, as well as xB) with their respective index relative to the basis
          let gammas = [((xB ! i) / (w ! i), i) | i <- basisInd, w ! i > 0]
          -- find minimum and its index in the basis
          let (gamma, il) = minimumBy (compare `on` fst) gammas
          -- update x and basis
          let xB' = xB - scale gamma w
          -- update index il (basis index) in basis with j (absolute index)
          let basis' = zipWith (\b k -> (if k == il then j else b)) basis basisInd
          let x' = assoc (size x) 0 ((j, gamma):(basis `zip` toList xB'))
          simplexIt a x' b c basis'
