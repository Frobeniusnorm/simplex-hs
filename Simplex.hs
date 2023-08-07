module Simplex where
import Selection
import Numeric.LinearAlgebra.Data


data SimplexResult = SimplexUnbounded | SimplexResult { x :: Vector R, basis :: [Int], iterations :: Int } deriving Show
