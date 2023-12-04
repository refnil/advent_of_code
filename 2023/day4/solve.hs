import Data.Char
import Data.List
import Data.Set qualified as S
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

number :: Parser Int
number = do
  digits <- map digitToInt <$> many1 digit
  return $ foldl (\current new -> current * 10 + new) 0 digits

data Card = Card
  { winning :: S.Set Int,
    have :: S.Set Int
  }
  deriving (Show)

type Content = [Card]

parseCard :: Parser Card
parseCard = do
  manyTill anyChar (char ':')
  spaces
  winning <- S.fromList <$> endBy1 number spaces
  char '|'
  spaces
  have <- S.fromList <$> endBy1 number spaces
  return $ Card winning have

parserFile :: Parser Content
parserFile = many1 parseCard

value :: Int -> Int
value 0 = 0
value n = 2 ^ (n - 1)

toTuple :: Card -> (S.Set Int, S.Set Int)
toTuple Card {winning, have} = (winning, have)

calcRealWin :: Int -> [Int] -> Int
calcRealWin 0 _ = 1
calcRealWin n cards = 1 + sum (take n (reverse cards))

solve1, solve2 :: Content -> Int
solve1 = sum . map (value . S.size . uncurry S.intersection . toTuple)
solve2 content =
  let winnings = traceShowId $ map (S.size . uncurry S.intersection . toTuple) content
      realWinning = zipWith calcRealWin (reverse winnings) (inits realWinning)
   in sum (traceShowId realWinning)

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
