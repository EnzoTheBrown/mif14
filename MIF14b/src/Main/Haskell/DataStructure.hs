module Main.Haskell.DataStructure where


data EDB = EDB [[String]] deriving Show
data IDB = IDB [[String]] deriving Show
data MAPPING = MAPPING [([String], [[String]])] deriving Show
data START = START EDB IDB MAPPING deriving Show

data NEIGHBOURS = NEIGHBOURS [String] deriving Show
data NODE = NODE (String, String, String, NEIGHBOURS) deriving Show

