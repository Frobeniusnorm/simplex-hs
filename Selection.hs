module Selection where
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import Debug.Trace
-- a -> z -> N -> j
type PricingFct = Matrix R -> Vector R -> [Int] -> Int

type RatioTestFct = Matrix R -> Vector R -> Vector R -> [Int] -> (Double, Int)

dantzigPricing :: PricingFct
dantzigPricing a z n = do
  -- simpler as it looks: we need the actual index, but only have zn, so we have to zip the indices in n with actual indices to index zn, but want the index in n
  let indexedZ = zipWith (\i j -> (z ! i, j)) [0 .. (snd (size a) - fst (size a) - 1)] n
  snd $ minimumBy (compare `on` fst) (filter (\zj -> fst zj < 0) indexedZ)

dantzigRatioTest :: RatioTestFct
dantzigRatioTest a xB w b = do
  let basisInd = [0 .. (fst (size a) - 1)]
  let gammas = [(w ! i, (xB ! i) / (w ! i), i) | i <- basisInd, w ! i > 0]
  let (_, min_gamma, _) = minimumBy (compare `on` (\(_, v, _) -> v)) gammas
  let all_minimum = filter (\(_, g, _) -> g >= min_gamma - 1e-9 && g <= min_gamma + 1e-9) gammas
  let (_, g, i) = maximumBy (compare `on` (\(v, _, _) -> v)) all_minimum
  (g, i)

dantzig = (dantzigPricing, dantzigRatioTest)

blandPricing :: PricingFct
blandPricing a z n = do
  let indexedZ = zipWith (\i j -> (z ! i, j)) [0 .. (snd (size a) - fst (size a) - 1)] n
  snd $ head (filter (\zj -> fst zj < 0) indexedZ)

blandRatioTest :: RatioTestFct
blandRatioTest a xB w b = do
  let basisInd = [0 .. (fst (size a) - 1)]
  let gammas = [((xB ! i) / (w ! i), i) | i <- basisInd, w ! i > 0]
  let (min_gamma, _) = minimumBy (compare `on` fst) gammas
  let all_minimum = filter (\(g, _) -> g >= min_gamma - 1e-9 && g <= min_gamma + 1e-9) gammas
  fst $ minimumBy (compare `on` snd) (all_minimum `zip` b)

bland = (blandPricing, blandRatioTest)

  
