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
import qualified Data.List as L
import qualified Data.Map.Strict as M

type Input = [Int]

parse :: String -> Input
parse [] = []
parse (',':rest) = parse rest
parse input = 
    let (number, rest) = break (==',') input
     in (read number) : parse rest

calcGas :: Input -> Int -> Int
calcGas input n = foldr (\i a -> a + abs (i - n)) 0 input

bestGas :: Input -> Int
bestGas input = 
    let minInput = minimum input
        maxInput = maximum input
        gases    = [(i, calcGas input i) | i <- [minInput..maxInput]]
        best     = L.minimumBy (compare `on` snd) gases
     in snd best


solve :: Input -> Maybe Int
solve = Just . bestGas

sum1toN :: Int -> Int
sum1toN i = (i * i + i) `div` 2

calcGas2 :: Input -> Int -> Int
calcGas2 input n = foldr (\i a -> a + sum1toN (abs (i - n))) 0 input

bestGas2 :: Input -> Int
bestGas2 input = 
    let minInput = minimum input
        maxInput = maximum input
        gases    = [(i, calcGas2 input i) | i <- [minInput..maxInput]]
        best     = L.minimumBy (compare `on` snd) gases
     in snd best

solve2 :: Input -> Maybe Int
solve2 = Just . bestGas2

getInput = parse <$> readFile "input"

main = do 
    input <- getInput
    print (solve input) -- Guess 347
    print (solve2 input)
