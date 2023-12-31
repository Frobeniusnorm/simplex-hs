{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.State
import Data.Char
import Debug.Trace
import Helper (naturalToStandard, standardToNatural, includeBounds, removeBounds)
import Numeric.LinearAlgebra
import Simplex (SimplexResult (SimplexResult, SimplexUnbounded), ProblemType (Standard, Natural))
import Simplex1 (simplex)
import Simplex2 (simplex)
import Simplex3 (blandSimplex, dantzigSimplex)
import System.Directory.Internal.Prelude (getArgs, toLower)
import qualified Simplex4

-- type, A, b, c, lower, upper
type Problem = (ProblemType, Matrix R, Vector R, Vector R, Vector R, Vector R)

type ParseError = [Char]

termMode mode rd prob rowcount = do
  let (probtype, a, b, c, l, u) = prob
  case mode of
    'a' -> (probtype, matrix (if rowcount < 0 then -rowcount else rowcount) (reverse rd), b, c, l, u)
    'b' -> (probtype, a, vector (reverse rd), c, l, u)
    'c' -> (probtype, a, b, vector (reverse rd), l, u)
    'l' -> (probtype, a, b, c, vector (reverse rd), u)
    'u' -> (probtype, a, b, c, l, vector (reverse rd))
    _ -> prob

parseProblem :: Either (String, Char, Problem, [Double], Int) ParseError -> Either (String, Char, Problem, [Double], Int) ParseError
parseProblem (Left ('f' : 'o' : 'r' : 'm' : ':' : t, mode, prob, rd, count)) = do
  let np@(_, a, b, c, l, u) = termMode mode rd prob count
  let probtype = [toLower x | x <- takeWhile (/= '\n') t, x `notElem` [' ', '\t']]
  case probtype of
    "standard" -> Left (t, '0', (Standard, a, b, c, l, u), [], 0)
    "natural" -> Left (t, '0', (Natural, a, b, c, l, u), [], 0)
    _ -> Left (t, mode, np, rd, count)
parseProblem (Left (m : ':' : t, mode, prob, rd, count)) = do
  let np@(r, a, b, c, l, u) = termMode mode rd prob count
  Left (t, m, np, [], 0)
parseProblem (Left ([], mode, prob, rd, count)) = do
  let np@(r, a, b, c, l, u) = termMode mode rd prob count
  Left ([], mode, np, [], 0)
parseProblem (Left (t, mode, prob, rd, count)) = do
  let nostr = takeWhile (\x -> isDigit x || x == '.' || x == '-') t
  let nextt = dropWhile (\x -> isDigit x || x == '.' || x == '-') t
  if null nostr
    then Left (drop 1 nextt, mode, prob, rd, if not (null t) && head t == '\n' then -count else count)
    else Left (nextt, mode, prob, (read nostr :: Double) : rd, if count < 0 then 1 else count + 1)
parseProblem (Right err) = Right err

readProblem s = do
  let foo = Left (s, '0', (Standard, matrix 0 [], vector [], vector [], vector [], vector []), [], 0) : map parseProblem foo
  case dropWhile (\case Left (x, _, _, _, _) -> not (null x); Right err -> False) foo !! 1 of
    Left res -> do
      let (_, _, r, _, _) = res
      r
    Right err ->
      error err

main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Usage: ./simplex [-c] [-v1 | -v2 | -vdantzig | ...] [file path | problem description]"
      putStrLn "if -c is set, it is expected that the only non-flag argument is the problem description in text form. Else a file path is expected to a file containting the description.\nVersions include:"
      putStrLn " -v1: Informal version"
      putStrLn " -v2: Basic version"
      putStrLn " -vdantzig | -vbland: Basic version with different pricing and ratio test techniques"
    else do
      let opt = ["-v1", "-v2", "-vdantzig", "-vbland", "-v4"]
      let fct = [Simplex1.simplex, Simplex2.simplex, Simplex3.dantzigSimplex, Simplex3.blandSimplex, Simplex4.simplex]
      if sum (map (\x -> if x `elem` args then 1 else 0) opt) > 1
        then putStrLn "Only one simplex version at a time possible!"
        else do
          let simplexfcts = filter (\x -> fst x `elem` args) (opt `zip` fct)
          let (version, simplexfct) = if not (null simplexfcts) then head simplexfcts else ("-vdantzig", dantzigSimplex)

          p@(t, a, b, c, l, u) <-
            if "-c" `elem` args
              then return $ readProblem (head (filter (\x -> head x /= '-') args))
              else do
                s <- readFile (head (filter (\x -> head x /= '-') args))
                return (readProblem s)
          putStrLn ("Solving " ++ show t ++ " problem (with simplex implementation " ++ drop 1 version ++ "):\na = " ++ show a ++ ",\nb = " ++ show b ++ ",\nc = " ++ show c ++ "\nbounds: " ++ show l ++ " <= x <= " ++ show u)
          -- TODO: include problem type
          -- let (ba, bb) = includeBounds t a b l u
          -- putStrLn ("a, b with bounds: " ++ show ba ++ ", " ++ show bb)
          let (na, nb, nc)
                | version == "-v1" = if t == Natural then (a, b, c) else standardToNatural a b c
                | t == Standard = (a, b, c)
                | otherwise = naturalToStandard a b c
          let smplx = simplexfct na nb nc
          case smplx of
            SimplexResult nx basis iterations -> do
              -- let ax = removeBounds a b nx l u
              let cost = do
                    let costv = nc <.> nx
                    if version == "-v1" && t == Standard || t == Natural then -costv else costv
              putStrLn ("Solution with cost = " ++ show cost ++ ",\nx = " ++ show (subVector 0 (size c) nx) ++ " (Calculation took " ++ show iterations ++ " iterations)")
            SimplexUnbounded -> putStrLn "Unbounded"
