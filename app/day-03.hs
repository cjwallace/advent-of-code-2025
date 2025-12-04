import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

bestDigit :: Int -> [Int] -> (Int, Int)
bestDigit n bank = (joltage, index)
  where
    joltage = maximum $ take (length bank - (n - 1)) bank
    index = fromJust (elemIndex joltage bank)

maxJoltage :: Int -> [Int] -> String
maxJoltage 0 _ = ""
maxJoltage _ [x] = show x
maxJoltage n bank =
  let (digit, index) = bestDigit n bank
      (_, remainder) = splitAt (index + 1) bank
   in show digit ++ maxJoltage (n - 1) remainder

solve :: [[Int]] -> (Int, Int)
solve banks = (part1, part2)
  where
    part1 = sum $ map (read . maxJoltage 2) banks
    part2 = sum $ map (read . maxJoltage 12) banks

parse :: String -> [[Int]]
parse input = map (map digitToInt) $ lines input

main :: IO ()
main = interact (show . solve . parse)
