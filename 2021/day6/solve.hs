#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Maybe
import Debug.Trace
import Data.Bits
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map.Strict as M

type Input = [Int]
type Counter = M.Map Int Int

parse :: String -> Input
parse [] = []
parse (',':rest) = parse rest
parse input = 
    let (number, rest) = break (==',') input
     in (read number) : parse rest

inputToCounter :: Input -> Counter
inputToCounter = M.fromListWith (+) . map (,1)

nextDay :: Counter -> Counter
nextDay counter = M.insertWith (+) 8 (fromMaybe 0 $ counter M.!? 0) (M.mapKeysWith (+) changeKeys counter)
    where changeKeys 0 = 6
          changeKeys n = n - 1

solveDay :: Int -> Input -> Maybe Int
solveDay day input = Just $ M.foldr (+) 0 $ (iterate nextDay $ inputToCounter input) !! day

solve :: Input -> Maybe Int
solve = solveDay 80

solve2 :: Input -> Maybe Int
solve2 = solveDay 256

getInput = parse <$> readFile "input"

main = do 
    input <- getInput
    print (solve input) 
    print (solve2 input)
