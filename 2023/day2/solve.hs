import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Grab = Grab
  { red :: Int,
    blue :: Int,
    green :: Int
  }
  deriving (Show)

instance Semigroup Grab where
  (Grab r1 b1 g1) <> (Grab r2 b2 g2) = Grab (r1 + r2) (b1 + b2) (g1 + g2)

instance Monoid Grab where
  mempty = Grab 0 0 0

data Game = Game
  { gameId :: Int,
    grabs :: [Grab]
  }
  deriving (Show)

number :: Parser Int
number = do
  digits <- map digitToInt <$> many1 digit
  return $ foldl (\current new -> current * 10 + new) 0 digits

parseColor :: Parser Grab
parseColor = do
  n <- number
  spaces
  choice
    [ string "red" >> pure (Grab n 0 0),
      string "green" >> pure (Grab 0 0 n),
      string "blue" >> pure (Grab 0 n 0)
    ]

parseGrab :: Parser Grab
parseGrab = mconcat <$> sepEndBy1 parseColor (string ", ")

parseGame :: Parser [Game]
parseGame = many $ do
  string "Game "
  gameId <- number
  string ": "
  grabs <- sepEndBy1 parseGrab (string "; ")
  endOfLine
  return $ Game gameId grabs

filterGrab1 :: Grab -> Bool
filterGrab1 grab = red grab <= 12 && green grab <= 13 && blue grab <= 14

filter1 :: Game -> Bool
filter1 game = all filterGrab1 (grabs game)

minimumGrab :: Grab -> Grab -> Grab
minimumGrab grab1 grab2 =
  Grab
    { red = (max (red grab1) (red grab2)),
      blue = (max (blue grab1) (blue grab2)),
      green = (max (green grab1) (green grab2))
    }

minimumGrabs :: [Grab] -> Grab
minimumGrabs = foldr minimumGrab mempty

power :: Grab -> Int
power grab = red grab * green grab * blue grab

solve1, solve2 :: [Game] -> Int
solve1 games = sum . map gameId . filter filter1 $ games
solve2 games = sum . map (power . minimumGrabs . grabs) $ games

main = do
  result <- parseFromFile parseGame "input"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
