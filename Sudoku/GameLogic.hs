module GameLogic where

import Utilities (shuffle)
import Validation (isValid)
import Data.List (transpose, (\\), nub, minimumBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Function (on)

-- Update a grid with a new value
updateGrid :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateGrid r c val grid =
    take r grid ++ [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

-- Solve Sudoku using backtracking
solve :: [[Int]] -> Maybe [[Int]]
solve grid =
    if isComplete grid then Just grid
    else case selectMRVCell grid of
        Nothing -> Nothing
        Just (r, c, possibleVals) ->
            listToMaybe $ catMaybes [solve (updateGrid r c val grid) | val <- possibleVals]

-- Check if the grid is complete
isComplete :: [[Int]] -> Bool
isComplete grid = all (all (/= 0)) grid

-- Select the cell with the minimum remaining values
selectMRVCell :: [[Int]] -> Maybe (Int, Int, [Int])
selectMRVCell grid =
    let cells = [(r, c, possibleValues grid r c) | r <- [0..8], c <- [0..8], grid !! r !! c == 0]
        filteredCells = filter (\(_, _, vals) -> not (null vals)) cells
    in if null filteredCells then Nothing
       else Just $ minimumBy (compare `on` (\(_, _, vals) -> length vals)) filteredCells

-- Calculate possible values for a cell
possibleValues :: [[Int]] -> Int -> Int -> [Int]
possibleValues grid r c =
    let rowVals = grid !! r
        colVals = transpose grid !! c
        boxVals = getBoxValues grid r c
        usedVals = nub (rowVals ++ colVals ++ boxVals)
    in [1..9] \\ usedVals

-- Extract values from a 3x3 box
getBoxValues :: [[Int]] -> Int -> Int -> [Int]
getBoxValues grid r c =
    let startRow = (r `div` 3) * 3
        startCol = (c `div` 3) * 3
    in concat [take 3 (drop startCol (grid !! row)) | row <- [startRow..startRow + 2]]

-- Generate a random Sudoku puzzle
generateSudoku :: IO (Maybe [[Int]])
generateSudoku = do
    let emptyGrid = replicate 9 (replicate 9 0)
    case solve emptyGrid of
        Just grid -> Just <$> shuffleGrid grid
        Nothing -> return Nothing

-- Shuffle rows and columns within bands
shuffleGrid :: [[Int]] -> IO [[Int]]
shuffleGrid grid = do
    newRows <- mapM shuffleBand [take 3 grid, take 3 (drop 3 grid), drop 6 grid]
    let transposed = transpose $ concat newRows
    newCols <- mapM shuffleBand [take 3 transposed, take 3 (drop 3 transposed), drop 6 transposed]
    return $ transpose $ concat newCols

-- Shuffle a band of rows or columns
shuffleBand :: [[Int]] -> IO [[Int]]
shuffleBand band = do
    indices <- shuffle [0, 1, 2]
    return [band !! i | i <- indices]

-- Remove numbers from a grid to create a puzzle
removeNumbers :: Int -> [[Int]] -> IO [[Int]]
removeNumbers cluesToRemove grid = do
    positions <- generateUniquePositions cluesToRemove
    return $ foldl (\g (r, c) -> updateGrid r c 0 g) grid positions

-- Generate unique positions for number removal
generateUniquePositions :: Int -> IO [(Int, Int)]
generateUniquePositions n = do
    allPositions <- shuffle [(r, c) | r <- [0..8], c <- [0..8]]
    return $ take n allPositions

