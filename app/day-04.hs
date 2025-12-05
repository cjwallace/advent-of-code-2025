import Control.Lens (ix, (&), (.~))

type Grid = [String]

type Location = (Int, Int)

-- Pattern match because head will error on empty list
surroundingIndices :: Grid -> Location -> [Location]
surroundingIndices [] _ = []
surroundingIndices grid@(firstRow : _) (row, col) =
  [ (r, c)
  | r <- [row - 1 .. row + 1],
    r >= 0,
    c <- [col - 1 .. col + 1],
    c >= 0,
    r < length grid,
    c < length firstRow,
    (r, c) /= (row, col)
  ]

isPaper :: Grid -> Location -> Bool
isPaper grid (row, col) = (grid !! row) !! col == '@'

forkliftable :: Grid -> Location -> Bool
forkliftable grid location =
  let neighbours = surroundingIndices grid location
      accessible = length (filter (isPaper grid) neighbours) < 4
      paper = isPaper grid location
   in accessible && paper

forkliftableLocations :: Grid -> [Location]
forkliftableLocations [] = []
forkliftableLocations grid@(firstRow : _) =
  [ (r, c)
  | r <- [0 .. length grid - 1],
    c <- [0 .. length firstRow - 1],
    forkliftable grid (r, c)
  ]

removePaper :: Grid -> Location -> Grid
removePaper grid (row, col) =
  grid & ix row . ix col .~ '.'

countPaper :: Grid -> Int
countPaper grid = sum $ map (length . filter (== '@')) grid

removeBatch :: Grid -> Grid
removeBatch grid =
  let ixs = forkliftableLocations grid
      newRows = foldl removePaper grid ixs
   in newRows

countPaperRemoved :: Grid -> Int
countPaperRemoved grid =
  let states = iterate removeBatch grid
      pairs = zip states (drop 1 states)
      finalGrid = snd $ last $ takeWhile (uncurry (/=)) pairs
   in countPaper grid - countPaper finalGrid

solve :: Grid -> (Int, Int)
solve grid = (part1, part2)
  where
    part1 = length $ forkliftableLocations grid
    part2 = countPaperRemoved grid

parse :: String -> Grid
parse = lines

main :: IO ()
main = interact (show . solve . parse)
