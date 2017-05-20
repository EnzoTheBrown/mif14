module GenerateSQL where
import DataStructure
import MyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Parse
import Text.Regex
import Evaluation


-----------------------------------------

initHashGraph :: Map String [NODE]
initHashGraph = Map.fromList []

getNode :: Map String [NODE] -> String -> [NODE]
getNode map name = case Map.lookup name map of
  Nothing -> []
  Just value -> value

setNode :: Map String [NODE] -> String -> NODE -> Map String [NODE]
setNode map name node = Map.insert name (node:(getNode map name)) map

-----------------------------------------
removeFirst 0 x = x
removeFirst i (x:xs) = removeFirst (i-1) xs
get2and3 (NODE (_, a, b, _)) = litteral a ++ "." ++ b
get1and2 (a, b) = litteral a ++ " AS " ++ litteral b

getAsName :: [[String]] -> Int -> [(String, String)]
getAsName [] _ = []
getAsName ((x:_):xs) i = (x, x ++ show i) : getAsName xs (i+1)

getVariables_ :: [String] -> [String] -> [String]
getVariables_ [] vars = vars
getVariables_ (x:xs) vars = 
  if x `elem` vars 
    then getVariables_ xs vars
    else getVariables_ xs (x:vars)

getVariables :: [[String]] -> [String] -> [String]
getVariables [] vars = vars
getVariables ((x:xs):xss) vars =
  getVariables xss (getVariables_ xs vars)

indexFirstDifferent current ((NODE (_, a, b, _):nodes)) i =
  if a == current
    then indexFirstDifferent current nodes (i+1)
    else i

indexFirstDifferentByName current [] i = i-1
indexFirstDifferentByName current ((NODE (a, _, _, _):nodes)) i =
  if a == current
    then indexFirstDifferentByName current nodes (i+1)
    else i


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- create the Table script for the EDB -- 

createTable_ :: [String] -> Int -> String
createTable_ (x:[]) i = 
  "   " ++ (show i) ++ " VARCHAR(150)\n);\n"
createTable_ (x:xs) i = 
  "   " ++ (show i) ++ " VARCHAR(150),\n" ++ (createTable_ xs (i+1))

createTable :: EDB -> String
createTable (EDB []) = ""
createTable (EDB ((x:xs):xss)) = 
  "CREATE TABLE " ++ x ++ "(\n" ++ (createTable_ xs 1) ++ createTable (EDB xss) ++ ";"

nameInEDB :: String -> EDB -> Bool
nameInEDB  _ (EDB []) = False
nameInEDB x (EDB (y:ys)) =
  if x == (y!!0)
    then True
    else nameInEDB x (EDB ys)

unifyEDB :: EDB -> EDB -> EDB
unifyEDB (EDB []) edb = edb
unifyEDB (EDB (y:ys)) edb = 
  if nameInEDB (y!!0) edb
    then unifyEDB (EDB ys) edb
    else unifyEDB (EDB ys) (mergeEDB (EDB [y]) edb)


-- create the Insert script for the EDBs --

insertTable_ :: [String] -> String
insertTable_ (x:[]) = "'" ++ x ++ "'"
insertTable_ (x:xs) ="'" ++ x ++ "'" ++ ", " ++ insertTable_ xs

insertTable :: EDB -> String
insertTable (EDB []) = ""
insertTable (EDB ((x:xs):xss)) =
  "INSERT INTO " ++ x ++ " VALUE (" ++ (insertTable_ xs) ++ ");\n" ++ insertTable (EDB xss)


-----------------------------------------


setNodes :: Map String [NODE] -> String -> [String] -> [String] -> Int -> Int -> Map String [NODE]
setNodes map _ [] _ _ _ = map
setNodes map name (var:vars) list i j=
  setNode (setNodes map name vars list i (j + 1)) var (NODE (name, (name ++ (show i)), show j, (NEIGHBOURS list)))

initGraph :: [[String]] -> Int -> Map String [NODE]
initGraph [] _ = initHashGraph
initGraph ((head:vars):xs) i = setNodes (initGraph xs (i + 1)) head vars vars i 1

createView :: ([String], [[String]]) -> String
createView ((name:_), _) = "CREATE OR REPLACE VIEW AS " ++ name
--
select_ :: String -> [String] -> Map String [NODE] -> String
select_ asName (x:[]) map =  get2and3 ((getNode map x)!!(indexFirstDifferentByName asName (getNode map x) 0))
select_ asName (x:xs) map = get2and3 ((getNode map x)!!(indexFirstDifferentByName asName (getNode map x) 0)) ++ ", " ++ (select_ asName xs map)

select :: String -> [String] -> Map String [NODE] -> String
select asName x map = "SELECT " ++ select_ asName x map

--

with :: String -> String
with x = "WITH RECURSIVE " ++ x ++ " AS( "

--

from_ :: [(String, String)] -> Map String [NODE] -> String
from_ (x:[]) map = get1and2 x
from_ (x:xs) map = get1and2 x ++ ", " ++ from_ xs map

from :: [(String, String)] -> Map String [NODE] -> String
from x map = "FROM " ++ from_ x map

--

getRelated :: String -> NEIGHBOURS -> Map String [NODE] -> String
getRelated asName (NEIGHBOURS (x:[])) map = get2and3 ((getNode map x)!!(indexFirstDifferent asName (getNode map x) 0) )
getRelated asName (NEIGHBOURS (x:xs)) map = get2and3 ((getNode map x)!!(indexFirstDifferent asName (getNode map x) 0) ) ++ ", " ++ (getRelated asName (NEIGHBOURS xs) map)


where__ :: [NODE] -> Map String [NODE] -> String
where__ [] _ = ""
where__ (x:[]) map = ""
where__ ((NODE (a,b,c,d)):(NODE (e,f,g,h)):nodes) map = 
  if (isNeg e) /= 1
    then "AND " ++ b ++ "." ++ c ++ " = " ++ f ++ "." ++ g ++ "\n" ++ where__ ((NODE (e,f,g,h)):nodes) map
    else   "AND " ++ (getRelated (litteral e) h map) ++ " NOT IN (SELECT * FROM "++ (litteral e) ++")" ++ where__ ((NODE (a,b,c,d)):nodes) map


--      list des variables concernées
where_ :: [String] -> Map String [NODE] -> String
where_ [] map = "\n"
where_ (x:xs) map = where__ (getNode map x) map ++ where_ xs map

whereSql :: [String]  -> Map String [NODE] -> String
whereSql x map = "WHERE " ++removeFirst 4 (where_ x map)

createViewSql :: ([String], [[String]]) -> String
createViewSql (h, body) = do
  let m = initGraph body 1
  let vars = getVariables body []
  if True
    then createView (h, body) ++ "\n" ++ select (h!!0) vars m ++ "\n" ++ from (getAsName body 1) m ++ "\n" ++ whereSql vars m ++ ";"
    else ""

-----------------------------------------

createViewSqlRecursive :: ([String], [[String]]) -> ([String], [[String]]) -> String
createViewSqlRecursive (hh, hbody) (h, body) = do
  let m = initGraph body 1
  let hm = initGraph hbody 1
  let vars = getVariables body []
  let hvars = getVariables hbody []
  if True
    then createView (h, body) ++ "\n" ++ subRegex (mkRegex (h!!0)) (with (h!!0) ++ "\n" ++ select (hh!!0) (removeFirst 1 hh) hm ++ " " ++ from (getAsName hbody 1) hm ++ "\nUNION ALL\n(" ++ select (h!!0) (removeFirst 1 h)  m ++"\n  " ++from (getAsName body 1) m ++ "\n" ++ whereSql vars m ++ "))\n") ("rec_" ++ (h!!0))
    else ""

createViewSqlRecursivePositive :: ([String], [[String]]) -> ([String], [[String]]) -> String
createViewSqlRecursivePositive (hh, body) y = createViewSqlRecursive (hh, body) y ++ "SELECT * FROM " ++ (hh!!0) ++ ";"



createViewSqlRecursiveNegative :: ([String], [[String]]) -> ([String], [[String]]) -> ([String], [[String]]) -> String
createViewSqlRecursiveNegative x y (h, body) = do 
  let m = initGraph body 1
  let vars = getVariables body []
  if True
    then createViewSqlRecursive x y ++ select (h!!0) vars m ++ "\n" ++ from (getAsName body 1) m ++ "\n(" ++ whereSql vars m ++ ");"
    else ""











