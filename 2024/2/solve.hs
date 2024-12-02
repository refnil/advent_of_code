import Data.List (find, inits, tails)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)

parseLine :: String -> [Integer]
parseLine = map read . words

parseFile :: String -> [[Integer]]
parseFile = map parseLine . lines

data Safety = SafeInc | SafeDec | Unsafe

instance Semigroup Safety where
  (<>) SafeInc SafeInc = SafeInc
  (<>) SafeDec SafeDec = SafeDec
  (<>) _ _ = Unsafe

calcSafety :: Integer -> Integer -> Safety
calcSafety a b
  | a == b = Unsafe
  | abs (a - b) > 3 = Unsafe
  | a > b = SafeDec
  | otherwise = SafeInc

isSafe :: Safety -> Bool
isSafe Unsafe = False
isSafe _ = True

puzzle1Line :: [Integer] -> Safety
puzzle1Line report =
  let allSafety = zipWith calcSafety report (tail report)
   in sconcat $ fromList allSafety

puzzle1 :: [[Integer]] -> Int
puzzle1 lines = length . filter isSafe $ map puzzle1Line lines

reportWithProblemDampener :: [Integer] -> [[Integer]]
reportWithProblemDampener initialReport =
  let starts = inits initialReport
      ends = tails initialReport
   in zipWith (\s e -> s <> drop 1 e) starts ends

puzzle2Line :: [Integer] -> Safety
puzzle2Line report =
  let possibilities = reportWithProblemDampener report
      possibilityResult = find isSafe $ map puzzle1Line possibilities
   in fromMaybe Unsafe possibilityResult

puzzle2 :: [[Integer]] -> Int
puzzle2 lines = length . filter isSafe $ map puzzle2Line lines

main = do
  content <- readFile "input"
  let parsed = parseFile content

  print (puzzle1 parsed)
  print (puzzle2 parsed)
