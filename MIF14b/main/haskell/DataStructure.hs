module DataStructure where
data EDB = EDB [[String]] deriving Show
data IDB = IDB [[String]] deriving Show
data MAPPING = MAPPING [([String], [[String]])] deriving Show
data START = START EDB IDB MAPPING deriving Show


