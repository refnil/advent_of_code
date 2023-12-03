import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Number = Number
  { value :: Int,
    start :: SourcePos,
    end :: SourcePos
  }
  deriving (Show)

data Symbol = Symbol
  { symbol :: Char,
    pos :: SourcePos
  }
  deriving (Show)

type Content = ([Number], [Symbol])

number :: Parser Int
number = do
  digits <- map digitToInt <$> many1 digit
  return $ foldl (\current new -> current * 10 + new) 0 digits

parseNumber :: Parser Number
parseNumber = do
  start <- getPosition
  n <- number
  end <- getPosition
  return $ Number n start (incSourceColumn end (-1))

parseSymbol :: Parser Symbol
parseSymbol = do
  pos <- getPosition
  c <- oneOf "+-/*%=$@#&"
  return $ Symbol c pos

parseLine :: Parser Content
parseLine =
  mconcat
    <$> many1
      ( choice
          [ char '.' >> pure ([], []),
            (\n -> ([n], [])) <$> parseNumber,
            (\s -> ([], [s])) <$> parseSymbol
          ]
      )

parserFile :: Parser Content
parserFile = do
  content <- mconcat <$> endBy1 parseLine newline
  eof
  return content

inBoundary :: (SourcePos, SourcePos) -> SourcePos -> Bool
inBoundary (topLeft, bottomRight) point =
  let y = sourceLine point
      x = sourceColumn point
   in x >= sourceColumn topLeft
        && x <= sourceColumn bottomRight
        && y >= sourceLine topLeft
        && y <= sourceLine bottomRight

toBoundary :: Number -> (SourcePos, SourcePos)
toBoundary Number {start, end} = (incSourceLine (incSourceColumn start (-1)) (-1), incSourceLine (incSourceColumn end 1) 1)

isPart :: [Symbol] -> Number -> Bool
isPart symbols n =
  let boundary = toBoundary n
   in any (inBoundary boundary . pos) symbols

solve1, solve2 :: Content -> Int
solve1 (numbers, symbols) = sum . map value . filter (isPart symbols) $ numbers

type GearWithParts = (Symbol, [Number])

match :: Content -> [GearWithParts]
match (numbers, symbols) = map (\s -> (s, filter (isPart [s]) numbers)) symbols

ratio :: GearWithParts -> Int
ratio (Symbol {symbol = '*'}, [n1, n2]) = value n1 * value n2
ratio _ = 0

solve2 = sum . map ratio . match

main = do
  result <- parseFromFile parserFile "input"
  case result of
    Left err -> print err
    Right games -> do
      print games
      print (solve1 games)
      print (solve2 games)
