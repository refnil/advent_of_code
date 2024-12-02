import Data.List (find, inits, tails)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)

type Level = Integer

type Report = [Level]

type Data = [Report]

parseLine :: String -> Report
parseLine = map read . words

parseFile :: String -> Data
parseFile = map parseLine . lines

data Safety = SafeInc | SafeDec | Unsafe

instance Semigroup Safety where
  (<>) SafeInc SafeInc = SafeInc
  (<>) SafeDec SafeDec = SafeDec
  (<>) _ _ = Unsafe

calcSafety :: Level -> Level -> Safety
calcSafety a b
  | a == b = Unsafe
  | abs (a - b) > 3 = Unsafe
  | a > b = SafeDec
  | otherwise = SafeInc

isSafe :: Safety -> Bool
isSafe Unsafe = False
isSafe _ = True

puzzle1Line :: Report -> Safety
puzzle1Line report =
  let allSafety = zipWith calcSafety report (tail report)
   in sconcat $ fromList allSafety

puzzle1 :: Data -> Int
puzzle1 lines = length . filter isSafe $ map puzzle1Line lines

reportWithProblemDampener :: Report -> [Report]
reportWithProblemDampener initialReport =
  let starts = inits initialReport
      ends = tails initialReport
   in zipWith (\s e -> s <> drop 1 e) starts ends

puzzle2Line :: Report -> Safety
puzzle2Line report =
  let possibilities = reportWithProblemDampener report
      possibilityResult = find isSafe $ map puzzle1Line possibilities
   in fromMaybe Unsafe possibilityResult

puzzle2 :: Data -> Int
puzzle2 lines = length . filter isSafe $ map puzzle2Line lines

main = do
  content <- readFile "input"
  let parsed = parseFile content

  print (puzzle1 parsed)
  print (puzzle2 parsed)
