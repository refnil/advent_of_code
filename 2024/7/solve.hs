import Control.Monad (forM)
import Control.Monad.RWS (modify)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Foldable (forM_)
import Data.List (find, foldl', inits, partition, stripPrefix, tails)
import Data.List.NonEmpty (fromList)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import GHC.IO (unsafePerformIO)

data Operator = Operator Int [Int]
  deriving (Show)

type Input = [Operator]

parseLine :: String -> Operator
parseLine line =
  let (objective, ':' : equation) = break (== ':') line
   in Operator (read objective) (map read $ words equation)

parseFile :: String -> Input
parseFile = map parseLine . lines

puzzle1 :: Input -> Int
puzzle1 = sum . mapMaybe execOperator

puzzle2 :: Input -> Int
puzzle2 = sum . mapMaybe execOperator2

main = do
  content <- readFile "input"
  let parsed = parseFile content
  print parsed

  print (puzzle1 parsed)
  print (puzzle2 parsed)

possibilities :: [Int] -> [Int]
possibilities [] = []
possibilities [x] = [x]
possibilities (x : y : rest) = do
  op <- [(+), (*)]
  possibilities (x `op` y : rest)

execOperator :: Operator -> Maybe Int
execOperator (Operator target numbers) = if target `elem` possibilities numbers then Just target else Nothing

concatenate :: Int -> Int -> Int
concatenate x y = read $ show x ++ show y

possibilities2 :: [Int] -> [Int]
possibilities2 [] = []
possibilities2 [x] = [x]
possibilities2 (x : y : rest) = do
  op <- [(+), (*), concatenate]
  possibilities2 (x `op` y : rest)

execOperator2 :: Operator -> Maybe Int
execOperator2 (Operator target numbers) = if target `elem` possibilities2 numbers then Just target else Nothing
