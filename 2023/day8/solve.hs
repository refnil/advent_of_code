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
  digits <- map digitToInt <$> many1 digit
  return $ foldl (\current new -> current * 10 + new) 0 digits

data Direction = DLeft | DRight
  deriving (Show)

type Location = String

data Content = Content
  { directions :: [Direction],
    crossroads :: M.Map Location (Location, Location)
  }
  deriving (Show)

location :: Parser Location
location = count 3 letter

crossroad :: Parser (Location, (Location, Location))
crossroad = do
  cur <- location
  string " = ("
  left <- location
  string ", "
  right <- location
  string ")"
  newline
  return (cur, (left, right))

parserFile :: Parser Content
parserFile =
  do
    directions <- many1 ((DLeft <$ char 'L') <|> (DRight <$ char 'R'))
    newline
    newline
    m <- M.fromList <$> many1 crossroad
    return $ Content directions m

path :: Location -> Content -> [Location]
path start Content {directions, crossroads} =
  let d' = cycle directions
      l' =
        start
          : zipWith
            ( \l d -> case d of
                DLeft -> fst (crossroads M.! l)
                DRight -> snd (crossroads M.! l)
            )
            l'
            d'
   in tail l'

finish :: Eq a => a -> [a] -> Bool
finish e l = last l == e

solve1, solve2 :: Content -> Int
solve1 = (+ 1) . length . takeWhile (/= "ZZZ") . path "AAA"
solve2 content = (+ 1) . length . takeWhile (not . all (finish 'Z')) . transpose . map (\s -> path s content) . filter (finish 'A') $ M.keys (crossroads content)

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
