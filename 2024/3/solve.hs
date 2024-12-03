{-# LANGUAGE ViewPatterns #-}
import Data.List (find, inits, tails, stripPrefix)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Char (isDigit)

type Data = [Token]

parseFile :: String -> Data
parseFile = toTokens

puzzle1 :: Data -> Int
puzzle1  = sum . eval 

puzzle2 :: Data -> Int
puzzle2 = sum . eval2

main = do
  content <- readFile "input"
  let parsed = parseFile content

  print (puzzle1 parsed)
  print (puzzle2 parsed)

data Token = Mul | OpenParens | CloseParens | Comma | Do | Dont | Num Int | Invalid
  deriving (Show, Eq)

when :: String -> String -> Maybe String
when = stripPrefix

toTokens :: String -> Data
toTokens [] = []
toTokens (when "do()" -> Just rest) = Do : toTokens rest
toTokens (when "don't()" -> Just rest) = Dont : toTokens rest
toTokens (when "mul" -> Just rest) = Mul : toTokens rest
toTokens (',':xs) = Comma : toTokens xs
toTokens ('(':xs) = OpenParens : toTokens xs
toTokens (')':xs) = CloseParens : toTokens xs
toTokens ss = 
  let (digits, rest) = span isDigit ss
   in case digits of
    [] -> Invalid : toTokens (tail rest)
    _ -> Num (read digits) : toTokens rest

eval :: Data -> [Int]
eval (Mul:OpenParens:(Num a):Comma: (Num b):CloseParens:xs) = a * b: eval xs
eval (_:ts) =  eval ts
eval [] = []

eval2 :: Data -> [Int]
eval2 (Mul:OpenParens:(Num a):Comma:(Num b):CloseParens:xs) = a * b: eval2 xs
eval2 (Dont:ts) = eval2Dont ts
eval2 (_:ts) =  eval2 ts
eval2 [] = []

eval2Dont :: Data -> [Int]
eval2Dont (Do:ts) = eval2 ts
eval2Dont (_:ts) = eval2Dont ts
eval2Dont [] = []