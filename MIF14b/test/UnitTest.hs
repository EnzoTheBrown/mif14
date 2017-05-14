module UnitTest where
import Test.HUnit
import Engine
import Data.Map (Map)
import qualified Data.Map as Map
import MyMap


mapTest = Map.fromList [("p",3),("q",2),("r",1),("s",1),("t",1)]
mapping3 = (MAPPING [(["p","$x"],[["NEGq","$x"],["r","$x"]]),(["p","$x"],[["NEGt","$x"],["q","$x"]]),(["q","$x"],[["s","$x"],["NEGt","$x"]]),(["r","$x"],[["t","$x"]])])
stratum = (initStrata initMap [["q","a"],["s","b"],["t","a"],["q","$x"],["s","$x"],["t","$x"]])
testSample3 = TestCase (assertEqual "for stratification "  mapTest $stratification ["p", "q", "r", "s", "t"] mapping3 stratum)

-- tests = TestList [TestLabel "test1" test1, TestLabel "test2" testSample3]


