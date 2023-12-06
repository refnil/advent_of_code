import Data.Char
import Data.List
import Data.Set qualified as S
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

number :: Parser Int
number = do
  digits <-
    map digitToInt
      <$> many1
        ( do
            d <- digit
            spaces
            return d
        )
  return $ foldl (\current new -> current * 10 + new) 0 digits

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Show)

type Content = [Race]

parserFile :: Parser Content
parserFile = do
  string "Time:"
  spaces
  times <- endBy1 number spaces
  -- newline
  string "Distance:"
  spaces
  distances <- endBy1 number spaces
  return $ zipWith Race times distances

waysToWin :: Race -> Int
waysToWin Race {time, distance} =
  let possibleRaces = [x * (time - x) | x <- [1 .. (time - 1)]]
      dropLower = dropWhile (\d -> d <= distance) possibleRaces
      takeHigher = takeWhile (\d -> d > distance) dropLower
   in length takeHigher

solve1, solve2 :: Content -> Int
solve1 races = foldl (*) 1 . map waysToWin $ races
solve2 _ = 0

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      print games
      print (solve1 games)
      print (solve2 games)
