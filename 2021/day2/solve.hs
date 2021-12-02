#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe
import Debug.Trace

data Instruction = Forward Int | Up Int | Down Int
    deriving Show

type Input = [Instruction]

parse :: String -> Input
parse = map (parseLine . words) . lines

parseLine :: [String] -> Instruction
parseLine ("forward":number:[]) = Forward (read number)
parseLine ("up":number:[]) = Up (read number)
parseLine ("down":number:[]) = Down (read number)
parseLine _ = undefined

solve :: Input -> Maybe Int
solve input = let (h, v) = foldr reducer (0,0) input in Just (h * v)

reducer :: Instruction -> (Int, Int) -> (Int, Int)
reducer (Forward n) (h,v) = (h+n,v)
reducer (Up n) (h,v) = (h, v-n)
reducer (Down n) (h,v) = (h, v+n)

solve2 :: Input -> Maybe Int
solve2 input = let (h, v, _) = foldr reducer2 (0,0,0) (reverse input) in Just (h * v)

reducer2 :: Instruction -> (Int, Int, Int) -> (Int, Int, Int)
reducer2 (Forward n) (h,v,a) = traceShowId (h + n,v + (a * n),a)
reducer2 (Up n)      (h,v,a) = traceShowId (h, v, a-n)
reducer2 (Down n)    (h,v,a) = traceShowId (h, v, a+n)

main = do 
    input <- parse <$> getContents
    print input
    print (solve input)
    print (solve2 input)

    

