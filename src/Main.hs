module Main where
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe

main :: IO ()
main = do
        day1::IO() 

day1 :: IO ()
day1 = do
    content <- readFile "day1.txt"
    let sortedPairs = sortAndPairSmallest content
    mapM_ print sortedPairs 

parseLine :: String -> Maybe (Integer, Integer) 
parseLine line =
    case map readMaybe (words line) of 
        [Just x, Just y] -> Just (x, y)
        _ -> Nothing 

sortAndPairSmallest :: String -> [(Integer, Integer)]
sortAndPairSmallest content = 
    let pairs = mapMaybe parseLine (lines content)
        firstColumn = sort (map fst pairs)
        secondColumn = sort (map snd pairs)
        paired = zip firstColumn secondColumn
    in paired