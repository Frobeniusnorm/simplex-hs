import Numeric.LinearAlgebra
import Simplex1 (simplex)
import Helper
main = do
  let a = (5 >< 2) [
            1, 1,
            0, 1,
            -1, 1,
            0, -1,
            -1, 0] :: Matrix R
  let b = vector [4, 3, 3, 0, 0]
  let c = vector [1, 2]
  print $ simplex a b c
  let (a2, b2, c2) = naturalToStandard a b c
  let (a3, b3, c3) = standardToNatural a2 b2 c2
  print $ simplex a3 b3 c3
