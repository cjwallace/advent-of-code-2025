import Data.List.Split (splitOn)

containsNRepeats :: Int -> Int -> Bool
containsNRepeats n x =
  let str = show x
      len = length str
      (seqLength, remainder) = len `divMod` n
      seq' = take seqLength str
      repeatingSeq = concat (replicate n seq')
   in seqLength >= 1
        && remainder == 0
        && repeatingSeq == str

twoRepeats :: (Int, Int) -> [Int]
twoRepeats (start, end) = filter (containsNRepeats 2) [start .. end]

nRepeats :: (Int, Int) -> [Int]
nRepeats (start, end) = filter (\x -> any (`containsNRepeats` x) [2 .. length $ show x]) [start .. end]

solve :: [(Int, Int)] -> (Int, Int)
solve ranges = (part1, part2)
  where
    part1 = sum $ ranges >>= twoRepeats
    part2 = sum $ ranges >>= nRepeats

parseRange :: String -> (Int, Int)
parseRange range = case splitOn "-" range of
  [start, end] -> (read start, read end)
  _ -> error "Invalid range"

parse :: String -> [(Int, Int)]
parse l = map parseRange $ splitOn "," l

main :: IO ()
main = interact (show . solve . parse)
