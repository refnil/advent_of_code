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
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
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
      [ ('1', One),
        ('2', Two),
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

same :: Eq a => [a] -> Bool
same [] = False
same (a : as) = all (== a) as

sameSplit :: Eq a => Int -> [a] -> Bool
sameSplit at as =
  let (start, end) = splitAt at as
   in same start && same end

cardToType :: [Value] -> Type
cardToType cards = inner (sort cards)
  where
    inner cs | same cs = FiveT
    inner cs | same (tail cs) || same (init cs) = FourT
    inner cs | sameSplit 2 cs || sameSplit 3 cs = Full
    inner (a : b : c : _) | same [a, b, c] = ThreeT
    inner (_ : b : c : a : _) | same [a, b, c] = ThreeT
    inner (_ : _ : c : a : b : _) | same [a, b, c] = ThreeT
    inner (a : aa : b : bb : _) | a == aa && b == bb = TwoPair
    inner (a : aa : _ : b : bb : _) | a == aa && b == bb = TwoPair
    inner (_ : a : aa : b : bb : _) | a == aa && b == bb = TwoPair
    inner (a : aa : _) | a == aa = Pair
    inner (_ : a : aa : _) | a == aa = Pair
    inner (_ : _ : a : aa : _) | a == aa = Pair
    inner (_ : _ : _ : a : aa : _) | a == aa = Pair
    inner _ = High

hand :: Parser Bid
hand = do
  values <- count 5 value
  space
  bid <- number
  newline
  pure (Bid (Hand (cardToType values) values) bid)

parserFile :: Parser Content
parserFile = many1 hand

solve1, solve2 :: Content -> Int
solve1 = sum . zipWith (*) [1 ..] . map valueBid . sortOn handBid
solve2 content = 0

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      print games
      print (solve1 games)
      print (solve2 games)
