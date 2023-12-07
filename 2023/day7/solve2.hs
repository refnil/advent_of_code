import Data.Char
import Data.Containers.ListUtils
import Data.List
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

data Value
  = Jack
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord)

data Type = High | Pair | TwoPair | ThreeT | Full | FourT | FiveT
  deriving (Show, Eq, Ord)

data Hand = Hand
  { handType :: Type,
    cards :: [Value]
  }
  deriving (Show, Eq, Ord)

data Bid = Bid
  { handBid :: Hand,
    valueBid :: Int
  }
  deriving (Show)

type Content = [Bid]

value :: Parser Value
value =
  choice $
    map
      (\(c, v) -> char c >> pure v)
      [ ('2', Two),
        ('3', Three),
        ('4', Four),
        ('5', Five),
        ('6', Six),
        ('7', Seven),
        ('8', Eight),
        ('9', Nine),
        ('T', Ten),
        ('J', Jack),
        ('Q', Queen),
        ('K', King),
        ('A', Ace)
      ]

(===) :: Value -> Value -> Bool
(===) Jack b = True
(===) a Jack = True
(===) a b = a == b

same :: [Value] -> Bool
same [] = True
same (Jack : as) = same as
same (a : as) = all (=== a) as

sameSplit :: Int -> [Value] -> Bool
sameSplit at as =
  let (start, end) = splitAt at as
   in same start && same end

countGroup :: Ord a => [a] -> [Int]
countGroup ls = reverse $ sort (inner (sort ls))
  where
    inner (a : b : rest) =
      let (cur : countRest) = inner (b : rest)
       in if a == b then (cur + 1) : countRest else 1 : cur : countRest
    inner [a] = [1]
    inner [] = []

classify :: Int -> [Int] -> Type
classify j (s : _) | j + s == 5 = FiveT
classify j (s : _) | j + s == 4 = FourT
classify 5 [] = FiveT
classify 2 (1 : _) = ThreeT
classify 1 [2, 2] = Full
classify 1 (1 : _) = Pair
classify 1 (2 : 1 : _) = ThreeT
classify 0 [3, 2] = Full
classify 0 (3 : _) = ThreeT
classify 0 (2 : 2 : _) = TwoPair
classify 0 (2 : _) = Pair
classify 0 (1 : _) = High
classify jacks count = seq (traceShowId (jacks, count)) undefined

cardToType :: [Value] -> Type
cardToType cards =
  let (jacks, rest) = partition (== Jack) cards
   in classify (length jacks) (countGroup rest)

hand :: Parser Bid
hand = do
  values <- count 5 value
  space
  bid <- number
  newline
  pure (Bid (Hand (cardToType values) values) bid)

parserFile :: Parser Content
parserFile = many1 hand

solve1 :: Content -> Int
solve1 = sum . zipWith (*) [1 ..] . map (valueBid . traceShowId) . sortOn handBid

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- let games = take 5 games'
      -- print games
      print (solve1 games)

-- wrong guesses
-- 252137391
