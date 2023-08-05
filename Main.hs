import System.Directory.Internal.Prelude (getArgs, toLower)
import Control.Monad.Trans.State
import Numeric.LinearAlgebra
import Data.Char
import Debug.Trace

data ProblemType = Standard | Natural deriving Show
type Problem = (ProblemType, Matrix R, Vector R, Vector R)

termMode mode rd prob rowcount = do
  let (probtype, a, b, c) = prob
  case mode of
    'a' -> (probtype, matrix (if rowcount < 0 then -rowcount else rowcount) (reverse rd), b, c)
    'b' -> (probtype, a, vector (reverse rd), c)
    'c' -> (probtype, a, b, vector (reverse rd))
    _ -> prob

parseProblem :: (String, Char, Problem, [Double], Int) -> (String, Char, Problem, [Double], Int)
parseProblem ('f':'o':'r':'m':' ':t, mode, prob, rd, count) = do
  let np@(_, a, b, c) = termMode mode rd prob count
  let probtype = [toLower x | x <- takeWhile (/= '\n') t, x `notElem` [' ', '\t']]
  case probtype of
    "standard" -> (t, '0', (Standard, a, b, c), [], 0)
    "natural" -> (t, '0', (Natural, a, b, c), [], 0)
    _ -> (t, mode, np, rd, count)

parseProblem (m:':':t, mode, prob, rd, count) = do
  let np@(r, a, b, c) = termMode mode rd prob count
  (t, m, (r, a, b, c), [], 0)

parseProblem ([], mode, prob, rd, count) = do
  let np@(r, a, b, c) = termMode mode rd prob count
  ([], mode, (r, a, b, c), [], 0)

parseProblem (t, mode, prob, rd, count) = do
  let nostr = takeWhile (\x -> isDigit x || x == '.') t
  let nextt = dropWhile (\x -> isDigit x || x == '.') t
  if null nostr then
    (drop 1 nextt, mode, prob, rd, if not (null t) && head t == '\n' then -count else count)
  else
    (nextt, mode, prob, (read nostr :: Double) : rd, if count < 0 then 1 else count + 1)

readProblem s = do
  let foo = (s, '0', (Standard, matrix 1 [1], vector [1], vector [1]), [], 0) : map parseProblem foo
  let (_, _, res, _, _) = dropWhile (\(x, _, _, _, _) -> not (null x)) foo !! 1
  res




main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: ./simplex [-c] [file path | problem (if -c flag present)]"
  else do
    p@(t, a, b, c) <- if "-c" `elem` args then
          return $ readProblem ( head (filter (\x -> head x /= '-') args) )

        else do
          s <- readFile ( head (filter (\x -> head x /= '-') args) )
          return (readProblem s)

    print p

