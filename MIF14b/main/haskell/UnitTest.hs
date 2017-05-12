module UnitTest where
import Test.HUnit
import Engine
import Data.Map (Map)
import qualified Data.Map as Map
import MyMap


-------------------------------------------------------
--   test bodyFull
------------------------------------------------------
edb1 = EDB [["link", "a", "b"]]
edb2 = EDB [["link", "a", "b"], ["metro", "a"], ["metro", "b"]]
setedb = mergeEDB (setEDB (Map.fromList [("$x", "a"), ("$y", "b")]) (MAPPING [(["metro", "$y"], [["link", "$x", "$y"]]), (["metro", "$x"], [["link", "$x", "$y"]])])) edb1
test1 = TestCase (assertEqual "bla" (show edb2) (show setedb))


edb_test2_1 = EDB [["link", "a", "b"]]
edb_test2_2 = EDB [["link", "a", "b"], ["metro", "a"], ["metro", "b"]]
setedb_test2 = mergeEDB (setEDB (Map.fromList [("$x", "a"), ("$y", "b")]) (MAPPING [(["metro", "$y"], [["link", "$x", "$y"]]), (["metro", "$x"], [["link", "$x", "$y"]])])) edb1
test2 = TestCase (assertEqual "bla" (show edb_test2_2) (show setedb_test2))




-- main = do
  










mapTest = Map.fromList [("p",3),("q",2),("r",1),("s",1),("t",1)]
mapping3 = (MAPPING [(["p","$x"],[["NEGq","$x"],["r","$x"]]),(["p","$x"],[["NEGt","$x"],["q","$x"]]),(["q","$x"],[["s","$x"],["NEGt","$x"]]),(["r","$x"],[["t","$x"]])])
stratum = (initStrata initMap [["q","a"],["s","b"],["t","a"],["q","$x"],["s","$x"],["t","$x"]])
testSample3 = TestCase (assertEqual "for stratification "  mapTest $stratification ["p", "q", "r", "s", "t"] mapping3 stratum)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" testSample3]


