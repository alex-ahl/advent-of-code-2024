module Main where
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe ( mapMaybe )
import qualified Data.Map.Strict as Map

main :: IO ()
main = do day1

day1 :: IO ()
day1 = do
    input <- readFile "day1.txt"
    let pairs = mapMaybe parseLine (lines input)
    print $ sum $ map findDistanceBetween $ sortAndPairSmallest pairs
    print $ sum $ calculateTotalSimilarityScore $ findDuplicates (map fst pairs) (map snd pairs)

parseLine :: String -> Maybe (Integer, Integer)
parseLine line =
    case map readMaybe (words line) of
        [Just x, Just y] -> Just (x, y)
        _ -> Nothing

sortAndPairSmallest :: [(Integer, Integer)] -> [(Integer, Integer)]
sortAndPairSmallest input =
    let firstColumn = sort (map fst input)
        secondColumn = sort (map snd input)
    in zip firstColumn secondColumn

findDistanceBetween :: (Integer, Integer) -> Integer
findDistanceBetween (x, y) = abs (x - y)

findDuplicates :: [Integer] -> [Integer] -> [(Integer, Integer)]
findDuplicates firstList secondList =
    let secondListCount = Map.fromListWith (+) [(x, 1) | x <- secondList]
    in [(x, Map.findWithDefault 0 x secondListCount) | x <- firstList]

calculateTotalSimilarityScore :: [(Integer, Integer)] -> [Integer]
calculateTotalSimilarityScore = map (uncurry (*))

