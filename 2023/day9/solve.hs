import Data.Char
import Data.Containers.ListUtils
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

number :: Parser Int
number = do
  sign <- option 1 (char '-' >> pure (-1))
  digits <- map digitToInt <$> many1 digit
  return $ sign * foldl (\current new -> current * 10 + new) 0 digits

newtype Content = Content [[Int]]
  deriving (Show)

parserFile :: Parser Content
parserFile = Content <$> endBy1 (sepBy1 number (char ' ')) newline

diffSerie :: [Int] -> [Int]
diffSerie (a : b : rest) = (b - a) : diffSerie (b : rest)
diffSerie _ = []

nextSequence :: [Int] -> Int
nextSequence serie =
  let diff = diffSerie serie
      allZero = all (== 0) diff
   in if allZero
        then head serie
        else last serie + nextSequence diff

solve1, solve2 :: Content -> Int
solve1 (Content cs) = sum . map nextSequence $ cs
solve2 (Content cs) = sum . map (nextSequence . reverse) $ cs

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
