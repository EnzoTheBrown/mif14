module Main.Haskell.UnitTest where
import System.IO
import Main.Haskell.DataStructure
import Main.Haskell.Parse
import Test.HUnit
import Main.Haskell.Engine
import Data.Map (Map)
import qualified Data.Map as Map
import Main.Haskell.MyMap
import Main.Haskell.Evaluation
import Debug.Trace
import Main.Haskell.Handling.EDBHandling



--applyTestStratification contents= do
--
--  let starts =  start (removeBlanks contents)
--  let edb_relations_ = init [ relation x | x <- schema $ head starts ]
--  let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
--  let (EDB edb_relations2) = execStratifiedEvaluation contents
--  TestCase (assertEqual ("for: \n" ++ example1) (length edb_relations2) (length edb_relations))


applyTest i contents example1= do
  let starts =  start (removeBlanks example1)
  let edb_relations_ = init [ relation x | x <- schema $ head starts ]
  let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
  let (EDB edb_relations2) = mergeEDB (execStratifiedEvaluation contents) (EDB edb_relations)
  TestCase (assertEqual ("for: \n" ++ example1) (length edb_relations2) (length edb_relations))


mainTest = do





  -----------------------------

  -----------------------------
  file1 <- openFile "ressources/sample2.txt"  ReadMode
  file11 <- openFile "ressources/UnitTestSample/sampleTest1.txt" ReadMode
  contents1 <- hGetContents file11
  example1 <- hGetContents file1
  
  let test1 = applyTest 1 example1 contents1
  -----------------------------
  file2 <- openFile"ressources/sample5.txt"  ReadMode
  file22 <- openFile "ressources/UnitTestSample/sampleTest2.txt" ReadMode
  contents2 <- hGetContents file22
  example2 <- hGetContents file2

  let test2 = applyTest 2 example2 contents2
  -----------------------------
  
  

  -----------------------------
  -----------------------------
   

  let tests = TestList [TestLabel "testEvaluation1" test1, TestLabel "testEvaluation2" test2]
  
  runTestTT tests

  













