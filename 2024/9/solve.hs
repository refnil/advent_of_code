import Control.Monad (forM)
import Control.Monad.RWS (modify)
import Control.Monad.ST (runST)
import Data.Char (isDigit, digitToInt)
import Data.Foldable (forM_)
import Data.List (find, foldl', inits, partition, stripPrefix, tails)
import Data.List.NonEmpty (fromList)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import GHC.IO (unsafePerformIO)

data File = File { size :: Int, iden :: Int}
  deriving Show

newtype Space = Space Int
  deriving (Show)

data Content = CFile File | CSpace Space
  deriving Show

type Input = [Content]



parseFile :: String -> Input
parseFile input = runST $ do
  reversedContentRef <- newSTRef []
  isFileRef <- newSTRef True
  nextIdRef <- newSTRef 0

  forM_ (map digitToInt $ filter isDigit input) $ \value -> do
    isFile <- readSTRef isFileRef
    if isFile
      then do
        newFile <- CFile . File value <$> readSTRef nextIdRef
        modifySTRef reversedContentRef (newFile :)
        modifySTRef nextIdRef (+1)
        writeSTRef isFileRef False
      else do
        let newSpace = CSpace $ Space value
        modifySTRef reversedContentRef (newSpace:)
        writeSTRef isFileRef True

  reverse <$> readSTRef reversedContentRef

main = do
  content <- readFile "input"
  let parsed = parseFile  content
  print parsed
  print (compact parsed (reverse parsed))

  print (puzzle1 parsed)
  print (puzzle2 parsed)

compact :: [Content] -> [Content] -> [File]
compact (CFile (File {iden=idenFront}): _)  (CFile (File {iden=idenBack, size}): _) | idenFront == idenBack = [File size idenBack]
compact front (CSpace _ : back) = compact front back
compact (CFile file : front) back = file : compact front back
compact (CSpace (Space spaceSize) : front) (CFile (File {size = fileSize, iden}): back) =
  case compare fileSize spaceSize of
    EQ -> File fileSize iden : compact front back
    LT -> File fileSize iden : compact (CSpace (Space (spaceSize - fileSize)) : front) back
    GT -> File spaceSize iden : compact front (CFile (File (fileSize - spaceSize) iden) : back)

filesToIds :: [File] -> [Int]
filesToIds (File {size, iden} : rest) = replicate size iden ++ filesToIds rest
filesToIds [] = []

puzzle1 :: Input -> Int
puzzle1 input = sum $ zipWith (*) [0..] $ filesToIds $ compact input (reverse input)

fit :: File -> [Content] -> [Content]
fit = undefined

compactFull :: [Content]  -> [Content]
compactFull content = foldr go content content
  where go :: Content -> [Content] -> [Content]
        go (CFile file) disk = fit file disk
        go (CSpace space) disk = disk


puzzle2 :: Input -> Int
puzzle2 = const 0