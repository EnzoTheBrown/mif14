module UnitTest where
import System.IO
import DataStructure
import Parse
import Test.HUnit
import Engine
import Data.Map (Map)
import qualified Data.Map as Map
import MyMap
import Evaluation
import Debug.Trace


applyTest contents example1= do
 
  let starts =  start (removeBlanks example1)
  let edb_relations_ = init [ relation x | x <- schema $ head starts ]
  let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
  let (EDB edb_relations2) = mergeEDB (execStratifiedEvaluation contents) (EDB edb_relations)
  TestCase (assertEqual ("for: \n" ++ example1) (length edb_relations2) (length edb_relations))


mainTest = do


  -----------------------------
  



  -----------------------------
  file1 <- openFile "../ressources/sample2.txt"  ReadMode 
  file11 <- openFile "../ressources/UnitTestSample/sampleTest1.txt" ReadMode
  contents1 <- hGetContents file11
  example1 <- hGetContents file1
  
  let test1 = applyTest example1 contents1
  -----------------------------
  file2 <- openFile"../ressources/sample5.txt"  ReadMode
  file22 <- openFile "../ressources/UnitTestSample/sampleTest2.txt" ReadMode
  contents2 <- hGetContents file22
  example2 <- hGetContents file2

  let test2 = applyTest example2 contents2
  -----------------------------
  
  

  -----------------------------
  -----------------------------
   

  let tests = TestList [TestLabel "testEvaluation1" test1, TestLabel "testEvaluation2" test2]
  
  runTestTT tests

  













