#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Maybe
import Debug.Trace
import Data.Bits
import Data.Monoid
import Data.Maybe
import Data.Function
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data SegmentTest = SegmentTest { test :: [Segment], output :: [Segment] }
 deriving Show
type Input = [SegmentTest]

data SegmentPart = Top | TopLeft | TopRight
             | Middle | BottomLeft | BottomRight
             | Bottom
 deriving (Show, Enum, Bounded, Eq, Ord)

type Segment = [SegmentPart]

toSegmentPart :: Char -> SegmentPart
toSegmentPart 'a' = Top
toSegmentPart 'b' = TopLeft
toSegmentPart 'c' = TopRight
toSegmentPart 'd' = Middle
toSegmentPart 'e' = BottomLeft
toSegmentPart 'f' = BottomRight
toSegmentPart 'g' = Bottom


toSegment :: String -> Segment
toSegment = map toSegmentPart

parse :: String -> Input
parse = map (parseLine . words) . lines

parseLine :: [String] -> SegmentTest
parseLine words =
    let (test, ("|":output)) = break (=="|") words
     in SegmentTest (map toSegment test) (map toSegment output)

countEasy :: [Segment] -> Int
countEasy input = length . filter id $ map (\w -> length w `elem` [2,4,3,7]) input


solve :: Input -> Maybe Int
solve input = Just . countEasy $ concatMap output input


type SS = S.Set SegmentPart
type Possibilities = M.Map SegmentPart SS
type Solution = M.Map SegmentPart SegmentPart

allSegment :: SS
allSegment = S.fromList [minBound .. maxBound]
allPossibilities :: Possibilities
allPossibilities = M.fromList $ map (,allSegment) [minBound..maxBound]

easyDeduction :: M.Map Int SS
easyDeduction = M.fromList 
    [ (1, S.fromList $ toSegment "cf")
    , (4, S.fromList $ toSegment "bcdf")
    , (7, S.fromList $ toSegment "acf")
    , (8, S.fromList $ toSegment "abcdefg")
    ]

applyEasyReduction :: Segment -> Possibilities -> Possibilities
applyEasyReduction s p = 
    let easy = M.lookup (length s) easyDeduction
     in maybe p (\restrict -> foldr (\part pos -> M.insertWith (S.intersection) part restrict pos) p s) easy
     

easyReduction :: SegmentTest -> Possibilities
easyReduction (SegmentTest t o) = traceShowId $ foldr applyEasyReduction allPossibilities (t ++ o)

searchSolution :: Possibilities -> Maybe Solution
searchSolution p = Nothing

toSolution :: Possibilities -> Maybe Solution
toSolution p = Nothing

applySolution :: SegmentTest -> Solution -> Int
applySolution t s = M.size s 

solveLine :: SegmentTest -> Maybe Int
solveLine i = applySolution i <$> (searchSolution . easyReduction $ i)

solve2 :: Input -> Maybe Int
solve2 input = sum <$> (sequence $ map solveLine input)

getInput = parse <$> readFile "input"

main = do 
    input <- getInput
    print (solve input)
    print (solve2 input)
