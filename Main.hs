{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.State
import Data.Char
import Debug.Trace
import Helper (naturalToStandard, standardToNatural)
import Numeric.LinearAlgebra
import Simplex1 (simplex)
import Simplex2 (simplex)
import Simplex3 (dantzigSimplex)
import System.Directory.Internal.Prelude (getArgs, toLower)

data ProblemType = Standard | Natural deriving (Show, Eq)

type Problem = (ProblemType, Matrix R, Vector R, Vector R)

type ParseError = [Char]

termMode mode rd prob rowcount = do
  let (probtype, a, b, c) = prob
  case mode of
    'a' -> (probtype, matrix (if rowcount < 0 then -rowcount else rowcount) (reverse rd), b, c)
    'b' -> (probtype, a, vector (reverse rd), c)
    'c' -> (probtype, a, b, vector (reverse rd))
    _ -> prob

parseProblem :: Either (String, Char, Problem, [Double], Int) ParseError -> Either (String, Char, Problem, [Double], Int) ParseError
parseProblem (Left ('f' : 'o' : 'r' : 'm' : ':' : t, mode, prob, rd, count)) = do
  let np@(_, a, b, c) = termMode mode rd prob count
  let probtype = [toLower x | x <- takeWhile (/= '\n') t, x `notElem` [' ', '\t']]
  case probtype of
    "standard" -> Left (t, '0', (Standard, a, b, c), [], 0)
    "natural" -> Left (t, '0', (Natural, a, b, c), [], 0)
    _ -> Left (t, mode, np, rd, count)
parseProblem (Left (m : ':' : t, mode, prob, rd, count)) = do
  let np@(r, a, b, c) = termMode mode rd prob count
  Left (t, m, (r, a, b, c), [], 0)
parseProblem (Left ([], mode, prob, rd, count)) = do
  let np@(r, a, b, c) = termMode mode rd prob count
  Left ([], mode, (r, a, b, c), [], 0)
parseProblem (Left (t, mode, prob, rd, count)) = do
  let nostr = takeWhile (\x -> isDigit x || x == '.' || x == '-') t
  let nextt = dropWhile (\x -> isDigit x || x == '.' || x == '-') t
  if null nostr
    then Left (drop 1 nextt, mode, prob, rd, if not (null t) && head t == '\n' then -count else count)
    else Left (nextt, mode, prob, (read nostr :: Double) : rd, if count < 0 then 1 else count + 1)
parseProblem (Right err) = Right err

readProblem s = do
  let foo = Left (s, '0', (Standard, matrix 1 [1], vector [1], vector [1]), [], 0) : map parseProblem foo
  case dropWhile (\case Left (x, _, _, _, _) -> not (null x); Right err -> False) foo !! 1 of
    Left res -> do
      let (_, _, r, _, _) = res
      r
    Right err ->
      error err

main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: ./simplex [-c] [-v1 | -v2 | -vdantzig] [file path | problem description (if -c flag present)]"
    else do
      let opt = ["-v1", "-v2", "-vdantzig"]
      let fct = [Simplex1.simplex, Simplex2.simplex, Simplex3.dantzigSimplex]
      if sum (map (\x -> if x `elem` args then 1 else 0) opt) > 1
        then putStrLn "Only one simplex version at a time possible!"
        else do
          let simplexfcts = filter (\x -> fst x `elem` args) (opt `zip` fct)
          let (version, simplexfct) = if not (null simplexfcts) then head simplexfcts else ("-vdantzig", dantzigSimplex)

          p@(t, a, b, c) <-
            if "-c" `elem` args
              then return $ readProblem (head (filter (\x -> head x /= '-') args))
              else do
                s <- readFile (head (filter (\x -> head x /= '-') args))
                return (readProblem s)
          putStrLn ("Solving " ++ show t ++ " problem (with simplex implementation " ++ drop 1 version ++ "):\na = " ++ show a ++ ",\nb = " ++ show b ++ ",\nc = " ++ show c)
          let (na, nb, nc)
                | version == "-v1" = if t == Natural then (a, b, c) else standardToNatural a b c
                | t == Standard = (a, b, c)
                | otherwise = naturalToStandard a b c
          let smplx = simplexfct na nb nc
          case smplx of
            Just nx -> do
              let cost = nc <.> nx
              putStrLn ("Solution with cost = " ++ show cost ++ ",\nx = " ++ show (subVector 0 (size c) nx))
            Nothing -> putStrLn "No solution"
