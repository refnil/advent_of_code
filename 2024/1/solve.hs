import Data.List (sort)
import Data.Map.Strict (findWithDefault, fromListWith)

parseLine :: String -> (Integer, Integer)
parseLine l = let [a, b] = words l in (read a, read b)

calcDiff :: Integer -> Integer -> Integer
calcDiff a b = abs (a - b)

process1 :: [String] -> Integer
process1 ls =
  let (list1, list2) = unzip . map parseLine $ ls
      sorted1 = sort list1
      sorted2 = sort list2
   in sum $ zipWith calcDiff sorted1 sorted2

process2 :: [String] -> Integer
process2 ls =
  let (list1, list2) = unzip . map parseLine $ ls
      list2Count = fromListWith (+) (map (,1) list2)
   in sum $ map (\l1 -> l1 * findWithDefault 0 l1 list2Count) list1

main = do
  fileLines <- lines <$> readFile "input"
  print (process1 fileLines)
  print (process2 fileLines)
