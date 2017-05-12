module MyMap where

import Data.Map (Map)
import qualified Data.Map as Map


initMap :: Map String Int
initMap = Map.fromList []

get :: Map String Int -> String -> Int
get map name = case Map.lookup name map of
    Nothing -> -1
    Just value -> value

set :: Map String Int -> String -> Int -> Map String Int
set map name value = Map.insert name value map

compareMaps :: Map String Int -> Map String Int -> [String] -> Bool
compareMaps _ _ [] = True
compareMaps map1 map2 (x:xs) = ((get map1 x) == (get map2 x)) && compareMaps map1 map2 xs

initMapVariables :: Map String String
initMapVariables = Map.fromList []

getVariable :: Map String String -> String -> String
getVariable map name = case Map.lookup name map of
    Nothing -> ""
    Just value -> value

setVariable :: Map String String -> String -> String -> Map String String
setVariable map name value = Map.insert name value map

