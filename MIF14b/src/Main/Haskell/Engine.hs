module Main.Haskell.Engine where
import Main.Haskell.Parse
import Main.Haskell.MyMap
import Main.Haskell.Stratification
import Main.Haskell.DataStructure
import Main.Haskell.Evaluation
import Main.Haskell.Stringify
import Main.Haskell.SQL.Data2SQL
import Main.Haskell.Handling.MAPPINGHandling
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import System.IO


execStratifiedEvaluation contents = do
    let starts = start $removeBlanks contents
   -- EDB
    let edb_relations_ = init [ relation x | x <- schema $ head starts ]
    let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
    let edb = (EDB edb_relations)
    -- IDB
    let idb_relations_ = init [ relation x | x <- schema $ starts!!1 ]
    let idb_relations = [(head x) : (atts $ last x) | x <- idb_relations_ ]
    let idb = (IDB idb_relations)
    -- MAPPING
    let mapping_tgds_ = init [ tgd x | x <- tgds $ starts!!2 ]
    let atoms = [ [head $ relation $ init $ last x] ++ (atts $ last $ relation $ init $ last x) | x <- mapping_tgds_]
    let queries = [ [ [head $ relation yy] ++ (atts $ last $ relation yy) |yy <- y ] | y <- [ init $query $ head x | x <- mapping_tgds_ ]]
    let mapping = (MAPPING $ myzip atoms queries)
    -- START
    let start = (START edb idb mapping)
    let strata = initMap
    let stratif = stratification (getNames idb_relations) mapping $ initStrata strata (edb_relations ++ idb_relations) 
    stratified_evaluation (length idb_relations) mapping edb stratif
    

launch x = do
    print ("###############  " ++ x ++ "  ###############")
    handle <- openFile x ReadMode  
    contents <- hGetContents handle  
    putStrLn contents
    let starts = start $removeBlanks contents
   -- EDB
    let edb_relations_ = init [ relation x | x <- schema $ head starts ]
    let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
    let edb = (EDB edb_relations)
    -- IDB
    let idb_relations_ = init [ relation x | x <- schema $ starts!!1 ]
    let idb_relations = [(head x) : (atts $ last x) | x <- idb_relations_ ]
    let idb = (IDB idb_relations)
    -- MAPPING
    let mapping_tgds_ = init [ tgd x | x <- tgds $ starts!!2 ]
    let atoms = [ [head $ relation $ init $ last x] ++ (atts $ last $ relation $ init $ last x) | x <- mapping_tgds_]
    let queries = [ [ [head $ relation yy] ++ (atts $ last $ relation yy) |yy <- y ] | y <- [ init $query $ head x | x <- mapping_tgds_ ]]
    let mapping = (MAPPING $ myzip atoms queries)
    -- START
    let start = (START edb idb mapping)
    let strata = initMap
    print edb
    print idb
    print mapping
    print (getNames idb_relations)
    let stratif = stratification (getNames idb_relations) mapping $ initStrata strata (edb_relations ++ idb_relations) 
    print "-- EDB --"
    let (EDB _edb) = stratified_evaluation (length idb_relations) mapping edb stratif
    putStrLn $toStringEDB _edb


    print $show (initMapByName mapping)

    hClose handle 
    print "###########################################################"


mainEngine = do
    launch "ressources/sample1.txt"
    launch "ressources/sample3.txt"
    launch "ressources/sample4.txt"
    launch "ressources/sample2.txt"
    launch "ressources/sample5.txt"

