import Numeric.LinearAlgebra
import Simplex2 (simplex)
import Simplex1 (simplex)
import Helper
ex1 = do
  let a = (5 >< 2) [
            1, 1,
            0, 1,
            -1, 1,
            0, -1,
            -1, 0] :: Matrix R
  let b = vector [4, 3, 3, 0, 0]
  let c = vector [1, 2]
  let (a2, b2, c2) = naturalToStandard a b c
  let (a3, b3, c3) = standardToNatural a2 b2 c2
  print $ Simplex2.simplex a2 b2 c2

ex2 = do
  let a = (13 >< 4) [
              -1, -2, 0, -1,
              1, 2, 0, 1,
              -1, 1, 0, 3,
              0, 1, -1, 0,
              0, -1, 1, 0, 
              0, 0, -1, 1,
              0, 0, 1, -1,
              -1, 0, 0, 0,
              1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, -1, 0,
              0, 0, 0, -1,
              0, 0, 0, 1
              ] :: Matrix R
  let b = vector [10, 30, 0, 6, 8, -10, 10, 0, 20, 10, 0, -10, 0]
  let c = vector [2, -1, 8, -2]
  print $ Simplex1.simplex a b c
  let (a2, b2, c2) = naturalToStandard a b c
  print $ Simplex2.simplex a2 b2 c2

main = do
  ex1
