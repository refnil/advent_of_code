import Data.Char (isDigit)
import Data.List (find, foldl', inits, partition, stripPrefix, tails)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import GHC.IO (unsafePerformIO)

type Prev = (Int, Int)

type Update = [Int]

data Data = Data
  { prev :: [Prev],
    update :: [Update]
  }
  deriving (Show)

parseUpdate :: String -> Update
parseUpdate line =
  let (number, rest) = break (== ',') line
   in case rest of
        [] -> [read number]
        (_ : restToParse) -> read number : parseUpdate restToParse

parseFile :: String -> Data
parseFile input =
  let l = lines input
      (prevLines, emptyLine : updateLines) = break (== "") l
      prev = map (\line -> let (as, b : bs) = break (== '|') line in (read as, read bs)) prevLines
      update = map parseUpdate updateLines
   in Data {prev = prev, update = update}

isValid :: Data -> Update -> Bool
isValid (Data {prev}) = isJust . foldl' go (Just S.empty)
  where
    go :: Maybe (S.Set Int) -> Int -> Maybe (S.Set Int)
    go Nothing _ = Nothing
    go (Just set) newNumber =
      let stillValid = newNumber `S.notMember` set
          newSet = S.union set $ S.fromList $ map fst $ filter (\(_, l) -> newNumber == l) prev
       in if stillValid then Just newSet else Nothing

getMiddle :: [a] -> a
getMiddle list = list !! (length list `quot` 2)

puzzle1 :: Data -> Int
puzzle1 i =
  let valids = filter (isValid i) (update i)
      middle = map getMiddle valids
   in sum middle

fix :: Data -> Update -> Update
fix (Data {prev}) = foldl' go []
  where
    go :: Update -> Int -> Update
    go [] i = [i]
    go current newNumber =
      let invalidInCurrent = S.fromList $ map snd $ filter (\(f, _) -> newNumber == f) prev
          (invalid, valid) = partition (`S.member` invalidInCurrent) current
       in valid ++ [newNumber] ++ invalid

puzzle2 :: Data -> Int
puzzle2 i =
  let invalids = filter (not . isValid i) (update i)
      middle = map (getMiddle . fix i) invalids
   in sum middle

main = do
  content <- readFile "input"
  let parsed = parseFile content
  -- print parsed

  print (puzzle1 parsed)
  print (puzzle2 parsed)

