import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Containers.ListUtils
import Data.Functor
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid
import Data.Set qualified as S
import Debug.Trace
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String

data Tile = NorthSouth | EastWest | NorthEast | NorthWest | SouthWest | SouthEast | Ground | Start
  deriving (Show, Eq)

data Pos = Pos {tile :: Tile, coord :: SourcePos}
  deriving (Show)

type Content = [Pos]

pos :: Parser Pos
pos = do
  sp <- getPosition
  tile <-
    choice
      [ char '|' $> NorthSouth,
        char '-' $> EastWest,
        char 'L' $> NorthEast,
        char 'J' $> NorthWest,
        char '7' $> SouthWest,
        char 'F' $> SouthEast,
        char '.' $> Ground,
        char 'S' $> Start
      ]
  pure $ Pos tile sp

parserFile :: Parser Content
parserFile = mconcat <$> endBy1 (many1 pos) newline

type Solve1 = State [S.Set SourcePos]

addToLoopPos :: SourcePos -> Int -> Int -> Solve1 ()
addToLoopPos sp x y = do
  let otherSp = incSourceColumn (incSourceLine sp x) y
  setWithOtherPos <- filter (`S.member` otherSP) <$> get
  case setWithOtherPos of
    [] -> modify (S.singleton sp :)
    [s] -> 
    other -> error ("Too many set: " <> show other)


addToLoop :: Pos -> Solve1 ()
addToLoop Pos {tile = Ground} = pure ()
addToLoop Pos {tile = Start} = pure ()
addToLoop Pos {tile = SouthEast, coord} = do
  addToLoopPos coord 0 1
  addToLoopPos coord 1 0
addToLoop Pos {tile = SouthWest, coord} = do
  addToLoopPos coord 0 1
  addToLoopPos coord (-1) 0
addToLoop Pos {tile = NorthWest, coord} = do
  addToLoopPos coord 0 (-1)
  addToLoopPos coord (-1) 0
addToLoop Pos {tile = NorthEast, coord} = do
  addToLoopPos coord 0 (-1)
  addToLoopPos coord 1 0
addToLoop Pos {tile = NorthSouth, coord} = do
  addToLoopPos coord 0 (-1)
  addToLoopPos coord 0 1
addToLoop Pos {tile = EastWest, coord} = do
  addToLoopPos coord (-1) 0
  addToLoopPos coord 1 0

solveState :: Content -> Solve1 Int
solveState cs = do
  mapM_ addToLoop cs
  let Just startPos = coord <$> find ((== Start) . tile) cs
  maximum . map S.size . filter (startPos `S.member`) <$> get

solve1, solve2 :: Content -> Int
solve1 cs = evalState (solveState cs) []
solve2 cs = 0

main = do
  result <- parseFromFile parserFile "input"
  -- result <- parseFromFile parserFile "example"
  case result of
    Left err -> print err
    Right games -> do
      -- print games
      print (solve1 games)
      print (solve2 games)
