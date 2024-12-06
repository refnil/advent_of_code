import Data.Char (isDigit)
import Data.List (find, foldl', inits, partition, stripPrefix, tails)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Debug.Trace (traceShow, traceShowId)
import GHC.IO (unsafePerformIO)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef)
import Control.Monad (forM)
import Data.Foldable (forM_)
import Control.Monad.RWS (modify)


data Direction = North | East | South | West
  deriving (Show, Eq, Ord)
type Coordinate = (Int, Int)

data Data = MkData
  { walls :: S.Set Coordinate
  , startingPosition :: Coordinate
  , xSize :: Int
  , ySize :: Int
  }
  deriving (Show)


parseFile :: String -> Data
parseFile input = runST $ do
  startingPostition <- newSTRef Nothing
  map <- newSTRef S.empty
  xSize <- newSTRef 0
  ySize <- newSTRef 0
  forM_ (zip [0..] (lines input)) $ \(y, line) ->
    forM_ (zip [0..] line) $ \(x, c) -> do
      case c of
        '#' -> modifySTRef map (S.insert (x, y))
        '^' -> writeSTRef startingPostition (Just (x, y))
        _ -> pure ()
      writeSTRef xSize x
      writeSTRef ySize y


  MkData <$> readSTRef map <*> (fromJust <$> readSTRef startingPostition) <*> readSTRef xSize <*> readSTRef ySize

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

unsafeMove :: Coordinate -> Direction -> Coordinate
unsafeMove (x, y) North = (x, y-1)
unsafeMove (x, y) East = (x+1, y)
unsafeMove (x, y) South = (x, y+1)
unsafeMove (x, y) West = (x-1, y)

move :: Coordinate -> Direction -> Int -> Int -> Maybe Coordinate
move coordinate direction xSize ySize =
  let newCoordinate@(x, y) = unsafeMove coordinate direction
   in if x >= 0 && x <= xSize && y >= 0 && y <= ySize
        then Just newCoordinate
        else Nothing

willGo :: Data -> S.Set Coordinate
willGo (MkData {startingPosition, walls, xSize, ySize}) =
  go  (S.singleton startingPosition) startingPosition North

  where go :: S.Set Coordinate -> Coordinate -> Direction -> S.Set Coordinate
        go visited position direction = do
            let newCoordinate = move position direction xSize ySize
            case newCoordinate of
              Nothing -> visited
              Just newCoordinate -> do
                let isWall = S.member newCoordinate walls
                if isWall
                  then
                    go visited position (turnRight direction)
                  else
                    go (S.insert newCoordinate visited) newCoordinate direction

puzzle1 :: Data -> Int
puzzle1 = S.size . willGo



timeLoop :: Data -> Coordinate -> Bool
timeLoop (MkData {startingPosition, walls, xSize, ySize}) (x,y) =
   traceShow (x,y) $ go (S.singleton (startingPosition, North)) startingPosition North
  where newWalls = S.insert (x, y) walls
        go :: S.Set (Coordinate, Direction) -> Coordinate -> Direction -> Bool
        go visited position direction = do
            let maybeAfterMove = move position direction xSize ySize
            case maybeAfterMove of
              Nothing -> False
              Just afterMove -> do
                let isWall = S.member afterMove newWalls
                let newDirection = if isWall then turnRight direction else direction
                let newCoordinate = if isWall then position else afterMove
                let visitedElem =  (newCoordinate, newDirection)
                let isTimeLoop = S.member visitedElem visited
                isTimeLoop || go (S.insert visitedElem visited) newCoordinate  newDirection


puzzle2 :: Data -> Int
puzzle2 input =
  let sp = startingPosition input
   in length $ [ position | position <- S.toList $ willGo input, position /= sp,  timeLoop input position]

main = do
  content <- readFile "input"
  let parsed = parseFile content
  -- print parsed

  -- print (puzzle1 parsed)
  print (puzzle2 parsed)

