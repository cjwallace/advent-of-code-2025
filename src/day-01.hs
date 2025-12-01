parseLine :: String -> Int
parseLine ('L':n) = -read n
parseLine ('R':n) = read n

parse :: String -> [Int]
parse =  map parseLine . lines

rotate :: Int -> Int -> Int
rotate position delta = (position + delta) `mod` 100

intoTicks :: Int -> [Int]
intoTicks x = replicate (abs x) (signum x)

countZeros :: [Int] -> Int
countZeros = length . filter (== 0) . scanl rotate 50

solve :: [Int] -> (Int, Int)
solve xs = (part1, part2)
  where
    part1 = countZeros xs

    -- Expand rotation out into individual ticks before counting
    part2 = countZeros $ concatMap intoTicks xs


main :: IO ()
main = interact (show . solve . parse)
