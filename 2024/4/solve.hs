import Data.List (find, inits, tails, stripPrefix, transpose)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Char (isDigit)
import Data.ByteString (count)

type Data = [[Char]]

parseFile :: String -> Data
parseFile = lines

puzzle1 :: Data -> Int
puzzle1 original = sum $ map countXMAS $ concatMap (\r -> [r, reverse r]) (mconcat [original, trans, diag_left_right, diag_right_left])
  where
    size = length original
    trans = transpose original
    diag_left_right = diagonals size original
    diag_right_left = diagonals size (reverse original)

puzzle2 :: Data -> Int
puzzle2 input = length occurences
  where
    size = length input
    occurences :: [(Int, Int)]
    occurences =
      [
        (i,j) |
        i <- [1..size-2],
        j <- [1..size-1],
        isCrossMax input size i j
      ]

main = do
  content <- readFile "input"
  -- content <- readFile "test"
  let parsed = parseFile content
  -- print parsed

  print (puzzle1 parsed)
  print (puzzle2 parsed)

countXMAS :: String -> Int
countXMAS [] = 0
countXMAS string =
  case stripPrefix "XMAS" string of
    Just rest -> 1 + countXMAS rest
    Nothing -> countXMAS (tail string)

diagonals :: Int -> [[a]] -> [[a]]
diagonals n matrix = map toDiagonal all
  where middle = [(a,a) | a <- [0..n-1]]
        left = [ [(a,a-i) | a <- [i..n-1]] | i <- [1..n-1]]
        top = [ [(a-i,a) | a <- [i..n-1]] | i <- [1..n-1]]
        all = middle:left ++ top
        -- toDiagonal :: [(Int, Int)] -> [a]
        toDiagonal = map (\(x,y) -> matrix !! x !! y)


isCrossMax :: Data -> Int -> Int -> Int -> Bool
isCrossMax input size i j = get (i, j) == 'A' && any checkPossibility possibilities
  where get (x,y) =
          if x >= 0 && y >= 0 && x < size && y < size
            then let val = input !! x !! y in val
            else ' '
        topLeft = (i-1, j-1)
        topRight = (i-1, j+1)
        bottomLeft = (i+1, j-1)
        bottomRight = (i+1, j+1)

        top = (topLeft, topRight)
        bottom = (bottomLeft, bottomRight)
        left = (topLeft, bottomLeft)
        right = (topRight, bottomRight)

        possibilities = [(top, bottom), (bottom, top), (left, right), (right, left)]

        checkPossibility ((fl1, fl2), (sl1, sl2)) =
          let result = get fl1 == 'M' && get fl2 == 'M' && get sl1 == 'S' && get sl2 == 'S'
           in result