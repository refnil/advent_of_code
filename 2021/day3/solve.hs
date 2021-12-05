#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe
import Debug.Trace
import Data.Bits
import Data.Monoid
import qualified Data.List as L

newtype Ior a = Ior a
    deriving Show

instance Bits a => Semigroup (Ior a) where
    (<>) (Ior a) (Ior b) = Ior (a .|. b)

instance (Num a, Bits a) => Monoid (Ior a) where
    mempty = Ior 0


size = 12
type Input = [String]

parse :: String -> Input
parse = map parseLine . lines

parseLine :: String -> String
parseLine = id

mkSum :: Int -> String -> (Sum Int, Sum Int)
mkSum i n = let t = n !! (size - 1 - i) == '1' in if t
                                        then (Sum 1, Sum 0)
                                        else (Sum 0, Sum 1)

elimSum :: Int -> (Sum Int, Sum Int) -> (Ior Int, Ior Int)
elimSum i (s1, s2) = let 
    si1 = getSum s1
    si2 = getSum s2
    n1 = if si1 > si2 then bit i else 0
    n2 = if si2 > si1 then bit i else 0
    in (Ior n1, Ior n2)
                           

test :: Input -> Int -> (Ior Int, Ior Int)
test ns i = elimSum i . mconcat . map (mkSum i) $ ns

solve :: Input -> Maybe Int
solve input = Just (gamma * epsilon)
    where l = length input
          (Ior gamma, Ior epsilon) = mconcat $ map (test input) [0..(size-1)]
          
compareOxygen :: Int -> Int -> Bool
compareOxygen n0 n1 = 
    case compare n0 n1 of
      EQ -> True
      LT -> True
      GT -> False

compareCO2 :: Int -> Int -> Bool
compareCO2 n0 n1 =
    case compare n0 n1 of
      EQ -> False
      LT -> False
      GT -> True

stringToNumber :: String -> Int
stringToNumber = foldl (\a v -> 2 * a + if v == '1' then 1 else 0) 0

solve2 :: Input -> Maybe Int
solve2 input = 
    let oxygen = stringToNumber $ findValue compareOxygen 0 input
        co2 = stringToNumber $ findValue compareCO2 0 input
     in Just (oxygen * co2)

findValue :: (Int -> Int -> Bool) -> Int -> [String] -> String
findValue _ _ [remaining] = traceShowId remaining
findValue which index potentials = 
    let (pot0, pot1) = L.partition (\p -> p !! index == '0') potentials
        l0 = length pot0
        l1 = length pot1
     in findValue which (index + 1) $ if which l0 l1 then pot1 else pot0

main = do 
    input <- parse <$> readFile "input"
    print (solve input)
    print (solve2 input)
