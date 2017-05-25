module Main.Haskell.Parse where
import Data.List
import Text.Regex.Posix    
import Text.Regex.Base
import Text.Regex          
import Text.Regex (splitRegex, mkRegex)
import Debug.Trace


isVariable :: String -> Bool
isVariable (a:b:_) = a == '$' && b `elem` '_' : ['a'..'z']++['A'..'Z']

removeBlanks :: String -> String
removeBlanks x = subRegex (mkRegex "\n| ") x ""

splitByPan :: String -> [String]
splitByPan = splitRegex (mkRegex "(EDB)|(IDB)|(MAPPING)")

splitByCloseParenthesis :: String -> [String]
splitByCloseParenthesis = splitRegex (mkRegex "\\)")

splitByCloseParenthesisAndComma :: String -> [String]
splitByCloseParenthesisAndComma = splitRegex (mkRegex "\\),?")


splitByOpenParenthesis :: String -> [String]
splitByOpenParenthesis = splitRegex (mkRegex "\\(")

splitByComma :: String -> [String]
splitByComma = splitRegex (mkRegex ",")

splitByDot :: String -> [String]
splitByDot = splitRegex (mkRegex "\\.")

splitByArrow :: String -> [String]
splitByArrow = splitRegex (mkRegex "->")

not_first :: [String] -> [String]
not_first (x:xs) = xs

isNeg :: String -> Int
isNeg (a:b:c:xs) = 
  if a == 'N' && b == 'E' && c == 'G'
    then 1
    else 0
isNeg _ = 0

myzip :: [[String]] ->[[[String]]] -> [([String], [[String]])]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x, y) : (myzip xs ys)

start :: String -> [String]
start x = not_first (splitByPan x)

schema :: String -> [String]
schema x = splitByCloseParenthesis x

query :: String -> [String]
query x = splitByCloseParenthesisAndComma x


relation :: String -> [String]
relation x = splitByOpenParenthesis x

atts :: String -> [String]
atts x = splitByComma x

tgds :: String -> [String]
tgds x = splitByDot x

tgd :: String -> [String]
tgd x = splitByArrow x

litteral :: String -> String
litteral (a:b:c:xs) = 
    if a == 'N' && b == 'E' && c =='G'
        then xs
        else a:b:c:xs
litteral a = a

