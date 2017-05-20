module Main.Haskell.Stratification where
import Main.Haskell.MyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Main.Haskell.Parse
import Main.Haskell.DataStructure
---------------------------------------------------------------
--          stratification
---------------------------------------------------------------

-- strataCorrect
-- args    : 
--         : MapStringInt  map predicat:value
--         : Int the maximum value of a statum
--         : the list of the different predicat
-- returns : True if each stratum are lesser than the number of predicats
--         : False otherwise
strataCorrect :: Map String Int -> Int -> [String] -> Bool
strataCorrect _ _ [] = True 
strataCorrect map n (x:xs) = get map x <= n && strataCorrect map n xs

-- initStrata
-- args    : 
--         : Map String Int a given map to fill with the key:value
--         : [[String]] the EDB content
--         : 
-- returns : a map with 1 as a value for each predicat 
--         : 
initStrata :: Map String Int -> [[String]] -> Map String Int
initStrata map [] = map
initStrata map (x:xs) = set (initStrata map xs) (head x) 1

-- for all negated subgoal of C with predicate q do
--   stratum[p] ← max(stratum[p], 1 + stratum[q])
-- AND
-- for all nonnegated subgoal of C with predicate q do
--   stratum[p] ← max(stratum[p], stratum[q])
stratification___ :: String -> [String] -> Map String Int -> Map String Int
stratification___ x [] map = map
stratification___ x (y:ys) map = 
  set (stratification___ (litteral x) ys map) (litteral x) $
    max (get map (litteral x)) (get map (litteral y) + isNeg y)

stratification__ :: String -> [String] -> Map String Int -> Map String Int
stratification__ x [] map = map
stratification__ x (y:ys) map = stratification___ x [y] (stratification__ x ys map) 

-- for all clause C in P with head predicate p do
stratification_ :: [([String], [[String]])] -> Map String Int -> Map String Int
stratification_ [] map = map
stratification_ (((x:_), y): yys) map = 
  stratification__ x [head yy | yy <- y] $ stratification_ yys map 

-- until no stratum changes or a stratum exceeds the predicate count in P
stratification :: [String] -> MAPPING -> Map String Int -> Map String Int
stratification keys (MAPPING x) map = do
  let n_map = stratification_ x map
  let cond = (strataCorrect n_map (length keys) keys)
  if compareMaps map n_map keys || not cond
    then n_map
    else stratification keys (MAPPING x) n_map



