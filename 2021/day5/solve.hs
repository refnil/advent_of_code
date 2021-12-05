#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Maybe
import Debug.Trace
import Data.Bits
import Data.Monoid
import qualified Data.List as L
import qualified Data.Map.Strict as M


data Point = Point { x :: Int, y :: Int } 
    deriving (Eq, Ord, Show)
data Line = Line { start :: Point, end :: Point}
    deriving Show

type Input = [Line]
type Map = M.Map Point Int

parse :: String -> Input
parse = map parseLine . lines

parseLine :: String -> Line
parseLine input = 
    let [f,_,s] = words input
        (x1, _:y1) = break (==',') f
        (x2, _:y2) = break (==',') s
     in Line (Point (read x1) (read y1)) (Point (read x2) (read y2))

isHorizontal :: Line -> Bool
isHorizontal Line{start = s, end = e} = y s == y e

isVertical :: Line -> Bool
isVertical Line{start = s, end = e} = x s == x e

fromToPoints :: Point -> Point -> [Point]
fromToPoints Point{x = x1, y = y1} Point{x = x2, y = y2} = 
    [
        Point x y | 
        x <- [min x1 x2..max x1 x2],
        y <- [min y1 y2..max y1 y2]
    ]

lineMap :: Line -> Map
lineMap Line{start = s, end = e} = M.fromList . map (,1) $ fromToPoints s e

enum :: Int -> Int -> [Int]
enum s e = if s <= e then [s..e] else [s,(s-1)..e]

lineMapDiag :: Line -> Map
lineMapDiag Line{start = p1, end = p2} = M.fromList . map (,1) $ zipWith Point (enum (x p1) (x p2)) (enum (y p1) (y p2))

solve :: Input -> Maybe Int
solve input = 
    let lines = filter (\l -> isHorizontal l || isVertical l) input
        vents = foldr (M.unionWith (+)) M.empty $ map lineMap lines
        big_vents = M.filter (>= 2) vents
     in Just (M.size big_vents)

          
solve2 :: Input -> Maybe Int
solve2 input = 
    let (lines, diag_lines) = L.partition (\l -> isHorizontal l || isVertical l) input
        vents = foldr (M.unionWith (+)) M.empty $ (map lineMap lines ++ map lineMapDiag diag_lines)
        big_vents = M.filter (>= 2) vents
     in Just (M.size big_vents)

getInput = parse <$> readFile "input"

main = do 
    input <- getInput
    print (solve input) 
    -- Guess: 1418 (L)
    print (solve2 input)

    

