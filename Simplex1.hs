module Simplex1 where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
import Numeric.LinearAlgebra
import Debug.Trace (trace)
import Prelude hiding ((<>))
import Data.List (minimumBy)
import Data.Function (on)
{-# LANGUAGE ScopedTypeVariables #-}
simplex :: Matrix R -> Vector R -> Vector R -> Maybe (Vector R)
{- | Implementation of the formal Simplex algorithm
Takes a maximization problem in natural form -}
simplex a b c = do
  -- highly inefficient !
  -- choose two last elements
  let active = [fst (size a) - 2, fst (size a) - 1]
  -- A_active * x = b
  let x = linearSolveLS (a ? active) (asColumn b ? active)
  simplexIt a (flatten x) b c active
  where
    simplexIt a x b c active = do
      -- calculate directions
      let e = \i -> tr (ident (fst $ size a) ? [i])
      let ws = [linearSolveLS (a ? active) (- e i ? active) | i <- active]      -- which improve the direction ? 
      let costs = [(c <.> flatten w, i) | (w, i) <- ws `zip` [0 .. length ws]] :: [(R, Int)]
      let imprv = filter (\w -> fst w > 0) costs
      -- check if there are improvements, if not -> already optimal
      if null imprv then
        Just x
      else do
        -- choose j and w as the first improvement cost
        let (_, wi) = head imprv
        let w = ws !! wi
        let j = active !! wi
        -- calculate inactive indices
        let inactive = filter (`notElem` active) [0 .. fst (size a) - 1]
        -- calculate lowest magnitude we can go in direction w
        let gammas = [(((b ! i) - (((a ? [i]) #> x) ! 0)) / flatten ((a ? [i]) <> w) ! 0, i) | i <- inactive]
        let validg = filter (\g -> fst g > 0) gammas
        -- check if there are valid gammas (gammas > 0), if not -> unbounded
        if null validg then
          Nothing
        else do
          let (gamma, k) = minimumBy (compare `on` fst) validg
          -- move along w with amount gamma, j leaves the basis, k enters it
          simplexIt a (x + (flatten w * realToFrac gamma)) b c (k:filter (/= j) active)
