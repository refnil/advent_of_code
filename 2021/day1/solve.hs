#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p ghc

{-# LANGUAGE ScopedTypeVariables #-}


solve :: [Int] -> Int
solve numbers = 
    let higher = zipWith (<) numbers (tail numbers)
     in foldr (\b p -> if b then p + 1 else p) 0 higher

sumBy3 :: [Int] -> Maybe [Int]
sumBy3 list@(x1:x2:x3:rest) = Just $ zipWith3 (\x y z -> x + y + z) list (tail list) (tail $ tail list)
sumBy3 _ = Nothing

solve2 :: [Int] -> Maybe Int
solve2 ns = solve <$> sumBy3 ns 

main = do 
    lines <- map read . lines <$> getContents
    print (solve lines)
    print (solve2 lines)

    

