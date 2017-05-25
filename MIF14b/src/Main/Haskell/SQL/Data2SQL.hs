module Main.Haskell.SQL.Data2SQL where
import Main.Haskell.SQL.GenerateSQL
import Main.Haskell.Evaluation 
import Main.Haskell.DataStructure
import Main.Haskell.Handling.MAPPINGHandling

commit :: String
commit = "\n\n"
  ++ "----------\n"
  ++ "COMMIT;"
  ++ "\n----------"
  ++ "\n\n"

handlePositiveness :: [([String], [[String]])] -> String
handlePositiveness mapping =
  if positive (MAPPING mapping)
    then createViewSqlRecursivePositive (mapping!!0) (mapping!!1)
    else createViewSqlRecursiveNegative (mapping!!0) (mapping!!1) (mapping!!2)

handleRecursiveness :: MAPPING -> String
handleRecursiveness (MAPPING mapping)  =
  if recursive (MAPPING mapping)
    then handlePositiveness mapping
    else createViewSql (mapping!!0)

data2Sql :: EDB -> MAPPING -> String
data2Sql edb mapping = 
  createTable (unifyEDB edb (EDB []))
  ++ commit 
  ++ insertTable edb 
  ++ commit 
  ++ handleRecursiveness mapping 

