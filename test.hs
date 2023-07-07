import Numeric.LinearAlgebra
import Simplex1 (simplex)

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
