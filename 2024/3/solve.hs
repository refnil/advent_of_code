import Data.List (find, inits, tails)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Char (isDigit)


data Token = Mul | OpenParens | CloseParens | Comma | Do | Dont | Num Int | Invalid
  deriving (Show, Eq)

type Data = String




toTokens :: Data -> [Token]
toTokens [] = []
toTokens ('d':'o':'(':')':xs) = Do : toTokens xs
toTokens ('d':'o':'n':'\'':'t':'(':')':xs) = Dont : toTokens xs
toTokens ('m':'u':'l':xs) = Mul : toTokens xs
toTokens (',':xs) = Comma : toTokens xs
toTokens ('(':xs) = OpenParens : toTokens xs
toTokens (')':xs) = CloseParens : toTokens xs
toTokens ss = 
  let (digits, rest) = span isDigit ss
   in case digits of
    [] -> Invalid : toTokens (tail rest)
    _ -> Num (read digits) : toTokens rest



eval :: [Token] -> [Int]
eval (Mul:OpenParens:(Num a):Comma: (Num b):CloseParens:xs) = a * b: eval xs
eval (t:ts) =  eval ts
eval [] = []

eval2 :: [Token] -> [Int]
eval2 (Mul:OpenParens:(Num a):Comma: (Num b):CloseParens:xs) = a * b: eval2 xs
eval2 (Dont:ts) = eval2Dont ts
eval2 (t:ts) =  eval2 ts
eval2 [] = []

eval2Dont :: [Token] -> [Int]
eval2Dont (Do:ts) = eval2 ts
eval2Dont (_:ts) = eval2Dont ts
eval2Dont [] = []

parseFile :: String -> Data
parseFile = id


puzzle1 :: Data -> Int
puzzle1 lines = sum . eval $ toTokens lines


puzzle2 :: Data -> Int
puzzle2 lines = sum . eval2 $ toTokens lines

main = do
  content <- readFile "input"
  let parsed = parseFile content

  print (puzzle1 parsed)
  print (puzzle2 parsed)
