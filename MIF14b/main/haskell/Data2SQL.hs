module Data2SQL where
import DataStructure
import MyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Parse


-----------------------------------------

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
  "CREATE TABLE " ++ x ++ "(\n" ++ (createTable_ xs 1) ++ createTable (EDB xss)


-- create the Insert script for the EDBs --

insertTable_ :: [String] -> String
insertTable_ (x:[]) = "'" ++ x ++ "'"
insertTable_ (x:xs) ="'" ++ x ++ "'" ++ ", " ++ insertTable_ xs

insertTable :: EDB -> String
insertTable (EDB []) = ""
insertTable (EDB ((x:xs):xss)) =
  "INSERT INTO " ++ x ++ " VALUE (" ++ (insertTable_ xs) ++ ");\n" ++ insertTable (EDB xss)

-----------------------------------------

get2nd (_,x,_,_) = x
get3rd (_,_,x,_) = x
getfst (x,_,_,_) = x

initSQLHash_ :: String 
             -> String 
	     -> [String] 
	     -> [String]
	     -> Int 
	     -> Map String [(String, String, String, [String])]
             -> Map String [(String, String, String, [String])]
initSQLHash_ _ _ [] _ _ map = map
initSQLHash_ name asName (x:xs) vars i map = 
  setSQLVariable (initSQLHash_ name asName xs vars (i+1) map) x (name, asName, (show i), (removeItem x vars) )


initSQLHash :: [[String]] 
            -> Int 
	    -> Map String [(String, String, String, [String])]
            -> Map String [(String, String, String, [String])]
initSQLHash [] _ map = map
initSQLHash ((head: vars):xs) i map = 
  initSQLHash_ head (head ++ (show i)) vars vars 1 (initSQLHash xs (i+1) map)

select :: [String]  
       -> Map String [(String, String, String, [String])]
       -> String
select (x:[]) map = 
  get2nd ((getSQLVariable map x)!!0) ++ "." ++ get3rd ((getSQLVariable map x)!!0)
select (x:xs) map = 
  get2nd ((getSQLVariable map x)!!0) ++ "." ++ get3rd ((getSQLVariable map x)!!0) ++ ", " ++ select xs map

insert_single x xs =
  if x `elem` xs
    then xs
    else x:xs

merge_single :: [(String, String)]
             -> [(String, String)]
	     -> [(String, String)]
merge_single [] ys = ys
merge_single (x:xs) ys =
  merge_single xs (insert_single x ys) 
  

from__ :: [(String, String, String, [String])]
       -> [(String, String)]
from__ [] = []
from__ ((a, b, c, _):ys) = 
  insert_single (a, b) (from__ ys)

from_ :: [(String, [(String, String, String, [String])])]
      -> [(String, String)]
from_ [] = []
from_ ((_, ys):xs) = 
  merge_single (from__ ys) (from_ xs)

from :: [(String, String)]
           -> String
from ((a, b):[]) = litteral(a) ++ " AS " ++ litteral b
from ((a, b): abs) = litteral(a) ++ " AS " ++ litteral b ++ ", " ++ from abs

-----------------------------------------

wherePositive_ :: [(String, String, String, [String])]
       -> String
wherePositive_ (_:[]) = ""
wherePositive_ ((a, b, c, _):(d, e, f, g):xs) =
  litteral(b) ++ "." ++ c ++ " = " ++ litteral(e) ++ "." ++ f ++ " AND " ++ wherePositive_ ((d,e,f, g):xs)

wherePositive :: [(String, [(String, String, String, [String])])]
       -> String
wherePositive [] = "1 = 1\n"
wherePositive ((_, ys):xs) =
  (wherePositive_ ys) ++ " AND " ++ (wherePositive xs)


createViewNonRecursivePositive ((h: vars), body) = do
  let m = (initSQLHash body 1 initMapSQL)
  if True
    then ("CREATE OR REPLACE VIEW " ++ h ++ " AS\nSELECT " ++ select vars m ++ "\nFROM " ++ from(from_ (Map.toList m)) ++ "\nWHERE " ++ wherePositive (Map.toList m) ++ ");\n")
    else ""

createViewsNonRecursivePositive (MAPPING []) = ""
createViewsNonRecursivePositive (MAPPING (x:xs)) = createViewNonRecursivePositive x ++ "\n" ++ createViewsNonRecursivePositive (MAPPING xs)

-----------------------------------------



