module Main where
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe ( mapMaybe )

main :: IO ()
main = do day1 

day1 :: IO ()
day1 = do readFile "day1.txt" >>= (print . sum . map findDistanceBetween . sortAndPairSmallest)

parseLine :: String -> Maybe (Integer, Integer) 
parseLine line =
    case map readMaybe (words line) of 
        [Just x, Just y] -> Just (x, y)
        _ -> Nothing 

sortAndPairSmallest :: String -> [(Integer, Integer)]
sortAndPairSmallest input =
    let pairs = mapMaybe parseLine (lines input)
        firstColumn = sort (map fst pairs)
        secondColumn = sort (map snd pairs)
    in zip firstColumn secondColumn

findDistanceBetween :: (Integer, Integer) -> Integer
findDistanceBetween (x, y) = abs (x - y)
    