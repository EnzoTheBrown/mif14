module Main.Haskell.Stringify where


toStringEDB_ :: [String]
             -> String
toStringEDB_ [] = ""
toStringEDB_ (x:[]) = x ++ ")\n"
toStringEDB_ (x:xs) = x ++ ", " ++ toStringEDB_ xs 


toStringEDB :: [[String]] 
            -> String
toStringEDB [] = ""
toStringEDB ((head:vars):xs) = 
  head ++ "(" ++ toStringEDB_ vars ++  toStringEDB xs


