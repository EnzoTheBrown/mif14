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


-- tests = TestList [TestLabel "test1" test1, TestLabel "test2" testSample3]

mainTest = do
  -----------------------------
  file1 <- openFile "../ressources/UnitTestSample/sampleTest1.txt" ReadMode
  file11 <- openFile "../ressources/sample2.txt" ReadMode
  contents <- hGetContents file11
  example1 <- hGetContents file1
  
  let starts =  start (removeBlanks example1)
  let edb_relations_ = init [ relation x | x <- schema $ head starts ]
  let edb_relations = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
  
  let (EDB edb_relations2) = mergeEDB (execStratifiedEvaluation contents) (EDB edb_relations)
  
  
  print $length edb_relations2
  print $length edb_relations 
  
  let test1 = TestCase (assertEqual ("for: \n" ++ example1) (length edb_relations2) (length edb_relations))
  -----------------------------
  file2 <- openFile "../ressources/UnitTestSample/sampleTest2.txt" ReadMode
  file22 <- openFile "../ressources/sample5.txt" ReadMode
  contents2 <- hGetContents file22
  example2 <- hGetContents file2
  
  let starts2 =  start (removeBlanks example1)
  let edb_relations2_ = init [ relation x | x <- schema $ head starts ]
  let edb_relations2 = [ (head x) : (atts $ last x) | x <- edb_relations_ ]
  
  let (EDB edb_relations22) = mergeEDB (execStratifiedEvaluation contents2) (EDB edb_relations2)
  
  
  
  let test2 = TestCase (assertEqual ("for: \n" ++ example2) (length edb_relations22) (length edb_relations2))
  -----------------------------
   

  let tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
  
  runTestTT tests

  hClose file11
  hClose file1
  
  













