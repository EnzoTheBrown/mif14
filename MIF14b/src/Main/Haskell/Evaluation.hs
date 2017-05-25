module Main.Haskell.Evaluation where
import Main.Haskell.Parse
import Main.Haskell.MyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Main.Haskell.DataStructure
import Main.Haskell.Handling.EDBHandling
import Main.Haskell.Handling.IDBHandling
import Main.Haskell.Handling.MAPPINGHandling

----------------------------------------------------------------
--          IDB handling
----------------------------------------------------------------

-- for the stratification, give all the predicat names
getNames :: [[String]] -> [String]
getNames [] = []
getNames ((x:_):xss) = x : getNames xss

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

slice_evaluation :: MAPPING -> EDB -> EDB
slice_evaluation (MAPPING mapping) (EDB edb) = do
  let fwd = forward_positive initMapVariables (MAPPING mapping) (EDB edb)
  let (EDB edbs) = mergeEDB (EDB edb) $setEDBs fwd (MAPPING mapping) (EDB edb)
  if (length edb) == (length edbs)
    then (EDB edbs)
    else (slice_evaluation (MAPPING mapping)  (EDB edbs))
  
------------------------------------

stratified_evaluation_ :: [MAPPING] -> EDB -> EDB
stratified_evaluation_ [] edb = edb
stratified_evaluation_ (mapping:mappings) edb = (stratified_evaluation_ mappings (slice_evaluation mapping edb))

stratified_evaluation :: Int -> MAPPING -> EDB -> Map String Int -> EDB
stratified_evaluation size mapping edb stratif =
  stratified_evaluation_ ((sliceMapping size 1 mapping stratif)) edb




