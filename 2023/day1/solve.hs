import Data.Maybe
import Debug.Trace
import Data.Char
import Data.List

parseInt :: Char -> Maybe Integer
parseInt c =
    let digit = toInteger $ ord c
     in if digit >= 48 && digit < 58
        then Just (digit - 48)
        else Nothing

solveLine :: String -> Maybe (Integer, Integer)
solveLine line = foldl reducer Nothing line
  where reducer :: Maybe (Integer, Integer) 
                -> Char
                -> Maybe (Integer, Integer)
        reducer Nothing c = maybe Nothing (\i -> Just (i,i)) (parseInt c)
        reducer prev@(Just (left, _)) c = maybe prev (\i -> Just (left, i)) (parseInt c) 

parseStart :: String -> Maybe Integer
parseStart ('0':_) = Just 0
parseStart ('1':_) = Just 1
parseStart ('2':_) = Just 2
parseStart ('3':_) = Just 3
parseStart ('4':_) = Just 4
parseStart ('5':_) = Just 5
parseStart ('6':_) = Just 6
parseStart ('7':_) = Just 7
parseStart ('8':_) = Just 8
parseStart ('9':_) = Just 9
parseStart str | "one" `isPrefixOf` str = Just 1
               | "two" `isPrefixOf` str = Just 2
               | "three" `isPrefixOf` str = Just 3
               | "four" `isPrefixOf` str = Just 4
               | "five" `isPrefixOf` str = Just 5
               | "six" `isPrefixOf` str = Just 6
               | "seven" `isPrefixOf` str = Just 7
               | "eight" `isPrefixOf` str = Just 8
               | "nine" `isPrefixOf` str = Just 9
               | otherwise = Nothing

solveLine2 :: String -> Maybe (Integer, Integer)
solveLine2 line = 
  let numbers = mapMaybe parseStart (tails line)
   in if null numbers
      then Nothing
      else let f = head numbers
               l = last numbers
            in Just (f, l)

solve1, solve2 :: String -> Maybe Integer
solve1 text = do
  let allLines = lines text
  let solvedLines :: [Maybe (Integer, Integer)] = map solveLine allLines
  sequenced <- sequence solvedLines
  let summed = sum (map (\(l, r) -> l * 10 + r) sequenced)
  return summed

solve2 text = do
  let allLines = lines text
  let solvedLines :: [Maybe (Integer, Integer)] = map solveLine2 allLines
  sequenced <- sequence solvedLines
  let summed = sum (map (\(l, r) -> l * 10 + r) sequenced)
  return summed

main = do
  content <- readFile "input" 
  print (solve1 content)
  print (solve2 content)
