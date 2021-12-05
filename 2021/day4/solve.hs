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

data Bingo = Bingo { rows :: [[Int]], columns :: [[Int]] }
    deriving Show

removeNumber :: Int -> Bingo -> Bingo
removeNumber number Bingo{rows=r, columns=c} = Bingo (filterNumber r) (filterNumber c)
    where filterNumber listOfList = map (filter (/= number)) listOfList

isWin :: Bingo -> Bool
isWin Bingo{rows=r, columns=c} = helper r || helper c
    where helper l = filter (== []) l /= []

data Input = Input [Int] [Bingo]
    deriving Show

parse :: String -> Input
parse input = 
    let (raw_numbers:raw_bingos) = filter (/= []) $ lines input
     in Input (parseNumbers raw_numbers) (parseBingos raw_bingos)

parseNumbers :: String -> [Int]
parseNumbers [] = []
parseNumbers (',':rest) = parseNumbers rest
parseNumbers line = 
    let (raw_number, rest) = break (==',') line
     in read raw_number:(parseNumbers rest)

parseBingos :: [String] -> [Bingo]
parseBingos [] = []
parseBingos lines = 
    let (bingo, rest) = splitAt 5 lines
     in (parseBingo bingo: parseBingos rest)

parseBingo :: [String] -> Bingo
parseBingo lines = 
    let rows = map (map read . words) lines
     in Bingo rows (L.transpose rows)

solve :: Input -> Maybe Int
solve (Input numbers bingos) = 
    let (winningNumber, winningBingo) = findWinningBingo numbers bingos
        wRows = rows winningBingo
     in Just (winningNumber * (sum $ mconcat wRows))

findWinningBingo :: [Int] -> [Bingo] -> (Int, Bingo)
findWinningBingo (n:ns) bingos = 
    let new_bingos = map (removeNumber n) bingos
        winBingos = filter isWin new_bingos
     in if not $ null winBingos then (n, head winBingos) else findWinningBingo ns new_bingos
          
solve2 :: Input -> Maybe Int
solve2 (Input numbers bingos) = 
    let (winningNumber, winningBingo) = findLastWinningBingo numbers bingos
        wRows = rows winningBingo
     in Just (winningNumber * (sum $ mconcat wRows))

findLastWinningBingo :: [Int] -> [Bingo] -> (Int, Bingo)
findLastWinningBingo (n:ns) bingos = 
    let new_bingos = map (removeNumber n) bingos
        (winBingos, notWinBingos) = L.partition isWin new_bingos
     in if ((not $ null winBingos) && null notWinBingos) then (n, head winBingos) else findLastWinningBingo ns notWinBingos

getInput = parse <$> readFile "input"

main = do 
    input <- getInput
    -- print input
    print (solve input) 
    print (solve2 input)
