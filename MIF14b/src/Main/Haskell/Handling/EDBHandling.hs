module Main.Haskell.Handling.EDBHandling where
import Main.Haskell.DataStructure
import Main.Haskell.MyMap
import Main.Haskell.Parse


----------------------------------------------------------------
--          EDB handling
----------------------------------------------------------------

-- compare 2 predicats
samePredicat :: [String] -> [String] -> Bool
samePredicat [] [] = True
samePredicat [] _ = False
samePredicat _ [] = False
samePredicat (x:xs) (y:ys) = (x == y) && samePredicat xs ys


-- check if a predicat is in the given EBD
inEDB :: [String] -> EDB -> Bool
inEDB _ (EDB []) = False
inEDB x (EDB (y:ys)) = samePredicat x y || inEDB x (EDB ys)


-- check if a a set of predicat are in the EDB
inEDBs :: [[String]] -> EDB -> Bool
inEDBs [] _ = True
inEDBs ((head:body):xs) edb =
  if isNeg head == 1
    then not (inEDB ((litteral head):body) edb) && inEDBs xs edb
    else (inEDB (head:body) edb) && inEDBs xs edb


-- distinctly merge 2 edb
mergeEDB :: EDB -> EDB -> EDB
mergeEDB edb (EDB []) = edb
mergeEDB (EDB x) (EDB (y:ys)) =
  if inEDB y (EDB x)
    then mergeEDB (EDB x) (EDB ys)
    else mergeEDB (EDB (y:x)) (EDB ys)



