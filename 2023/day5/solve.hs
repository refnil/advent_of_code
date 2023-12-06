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

data Interval = Interval
  { start :: Int,
    range :: Int
  }

newtype Intervals = Intervals [Interval]

type Map = Int -> Int

type MapSegment = Int -> First Int

data Content = Content
  { seeds :: [Int],
    seedSoil :: Map,
    soilFertilizer :: Map,
    fertilizerWater :: Map,
    waterLight :: Map,
    lightTemperature :: Map,
    temperatureHumidity :: Map,
    humidityLocation :: Map
  }

mapSegment :: Int -> Int -> Int -> Int -> First Int
mapSegment startInput startTarget range value =
  First $
    if value >= startInput && value < startInput + range
      then Just (startTarget + (value - startInput))
      else Nothing

parseMapSegment :: Parser MapSegment
parseMapSegment = do
  targetStart <- number
  spaces
  start <- number
  spaces
  range <- number
  newline
  return $ mapSegment start targetStart range

parseMap :: String -> String -> Parser Map
parseMap header1 header2 = do
  string header1
  string "-to-"
  string header2
  string " map:"
  newline
  segments <- many1 parseMapSegment
  newline
  return $ \input -> fromMaybe input (getFirst $ mconcat segments input)

parserFile :: Parser Content
parserFile =
  do
    string "seeds: "
    seeds <- sepBy1 number (char ' ')
    newline
    newline
    Content seeds <$> parseMap "seed" "soil"
    <*> parseMap "soil" "fertilizer"
    <*> parseMap "fertilizer" "water"
    <*> parseMap "water" "light"
    <*> parseMap "light" "temperature"
    <*> parseMap "temperature" "humidity"
    <*> parseMap "humidity" "location"

apply :: Content -> Int -> Int
apply Content {seedSoil, soilFertilizer, fertilizerWater, waterLight, lightTemperature, temperatureHumidity, humidityLocation} = humidityLocation . temperatureHumidity . lightTemperature . waterLight . fertilizerWater . soilFertilizer . seedSoil

extraSeeds :: [Int] -> [Int]
extraSeeds [] = []
extraSeeds (start : range : rest) = enumFromTo start (start + range - 1) ++ extraSeeds rest

solve1, solve2 :: Content -> Int
solve1 content = minimum (map (apply content) (seeds content))
solve2 content = minimum (map (apply content) (nubOrd . extraSeeds $ seeds content))

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
