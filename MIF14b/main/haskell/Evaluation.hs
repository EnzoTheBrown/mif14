module Evaluation where
import Parse
import MyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import DataStructure


----------------------------------------------------------------
--          EDB handling
----------------------------------------------------------------
samePredicat :: [String] -> [String] -> Bool
samePredicat [] [] = True
samePredicat [] _ = False
samePredicat _ [] = False
samePredicat (x:xs) (y:ys) = (x == y) && samePredicat xs ys

inEDB :: [String] -> EDB -> Bool
inEDB _ (EDB []) = False
inEDB x (EDB (y:ys)) = samePredicat x y || inEDB x (EDB ys)

inEDBs :: [[String]] -> EDB -> Bool
inEDBs [] _ = True
inEDBs ((head:body):xs) edb =
  if isNeg head == 1
    then not (inEDB ((litteral head):body) edb) && inEDBs xs edb
    else (inEDB (head:body) edb) && inEDBs xs edb

mergeEDB :: EDB -> EDB -> EDB
mergeEDB edb (EDB []) = edb
mergeEDB (EDB x) (EDB (y:ys)) =
  if inEDB y (EDB x)
    then mergeEDB (EDB x) (EDB ys)
    else mergeEDB (EDB (y:x)) (EDB ys)
----------------------------------------------------------------
--          MAPPING handling
----------------------------------------------------------------

containsNEG :: [[String]] -> Bool
containsNEG [] = False
containsNEG ((head:body):xs) = (isNeg head == 1) || containsNEG xs
----------------------------------------------------------------
--          Evaluating a Positive Datalog Program
----------------------------------------------------------------

-- MAPPING => tuple : (atom1, (atom2, atom3, atom4, ...) EDB+=atom1 si atom2&&atom3&&....
initVars :: [String] -> [String] -> Map String String
initVars [] _ = initMapVariables
initVars _ [] = initMapVariables
initVars (x:xs) (y:ys) = setVariable (initVars xs ys) x y

replaceVariableByConstant :: Map String String -> [String] -> [String]
replaceVariableByConstant map [] = []
replaceVariableByConstant map (x:xs) = 
  getVariable map x : replaceVariableByConstant map xs

replaceVariablesByConstants :: Map String String -> [[String]] -> [[String]]
replaceVariablesByConstants map [] = []
replaceVariablesByConstants map ((name:vars):xs) = 
 (name:replaceVariableByConstant map vars) : replaceVariablesByConstants map xs

setEDB :: Map String String -> MAPPING -> EDB -> EDB
setEDB _ (MAPPING []) _ = (EDB [])
setEDB map (MAPPING (((head:vars), body):xs)) edb = 
  if inEDBs (replaceVariablesByConstants map body) edb
    then  mergeEDB (EDB [head:(replaceVariableByConstant map vars)]) (setEDB map (MAPPING xs) edb)
    else mergeEDB (EDB []) (setEDB map (MAPPING xs) edb)


setEDBs :: [Map String String] -> MAPPING -> EDB -> EDB
setEDBs [] _ _ = (EDB [])
setEDBs (x:xs) mapping edb =
  mergeEDB (setEDB x mapping edb) (setEDBs xs mapping edb)


---------------------------------------------------
--------           Init the HashMap           -----
---------------------------------------------------


--    args: HashMap pour stocker les variables
--        : la liste des variables
--        : la liste des constantes
-- return : HashMap avec les variables mise à jour
setConstant :: Map String String -> [String] -> [String] -> Map String String
setConstant map [] [] = map
setConstant map (x:xs) (y:ys) = 
  if getVariable map y == "" || getVariable map y == x
    then setVariable (setConstant map xs ys) y x
    else setConstant map xs ys

--    args: HashMap pour stocker les variables
--        : liste des variables
--        : liste des atomes
-- return : liste des HashMap possibles en fonction des EDB choisis pour chaque variable
getPredicatsByName :: Map String String -> [String] -> [[String]] -> [Map String String]
getPredicatsByName map _ [] = [map]
getPredicatsByName map (name:vars) ((head:consts):xs) =
  if (litteral name) == head
    then (setConstant map consts vars) : (getPredicatsByName map (name:vars) xs)
    else getPredicatsByName map (name:vars) xs

--    args: HashMap pour stocker les variables
--        : liste des variables
--        : liste des atomes
-- return : liste des HashMap possibles en fonction des EDB choisis pour chaque variable
forward__ :: [Map String String] -> [String] -> [[String]] -> [Map String String]
forward__ (x:[]) mapping edbs = getPredicatsByName x mapping edbs
forward__ (x:xs) mapping edbs = (getPredicatsByName x mapping edbs) ++ (forward__ xs mapping edbs)

--    args: HashMap pour stocker les variables
--        : liste des variables
--        : liste des atomes
-- return : liste des HashMap possibles en fonction des EDB choisis pour chaque variable
forward_ :: Map String String -> [[String]] -> [[String]] -> [Map String String]
forward_ map (x:[]) edbs = forward__ [map] x edbs
forward_ map (x:xs) edbs = forward__ (forward_ map xs edbs) x edbs


forward_positive :: Map String String -> MAPPING -> EDB -> [Map String String]
forward_positive map (MAPPING []) (EDB edb) = []
forward_positive map (MAPPING ((h, body):ys)) (EDB edb) =
  (forward_ map (reverse body) edb) ++ (forward_positive map (MAPPING ys) (EDB edb))

------------------------------------
------------------------------------

forward_chaining :: MAPPING -> EDB -> Int -> EDB
forward_chaining (MAPPING mapping) (EDB edb) = do
  let fwd = forward_positive initMapVariables (MAPPING mapping) (EDB edb)
  let (EDB edbs) = setEDBs fwd (MAPPING mapping) (EDB edb)
  if (length edb) == (length edbs)
    then (EDB edbs)
    else forward_chaining (MAPPING mapping) (mergeEDB (EDB edb) (EDB edbs))
  
------------------------------------


