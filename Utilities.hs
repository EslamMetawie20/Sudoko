module Utilities where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List ( (\\), nub, transpose)

-- to shuffle it
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    randIndex <- generateRandom 0 (length xs - 1)
    let (front, (x:back)) = splitAt randIndex xs
    rest <- shuffle (front ++ back)
    return (x:rest)

-- Function to extract values from the corresponding 3x3 box
getBoxValues :: [[Int]] -> Int -> Int -> [Int]
getBoxValues grid r c =
    let startRow = (r `div` 3) * 3
        startCol = (c `div` 3) * 3
    in concat [ take 3 (drop startCol (grid !! row)) | row <- [startRow..startRow+2] ]

-- Function to calculate possible values for a cell based on Sudoku rules
possibleValues :: [[Int]] -> Int -> Int -> [Int]
possibleValues grid r c =
    let rowVals = grid !! r                       -- Extracts all values in row r
        colVals = transpose grid !! c           -- Extracts all values in column c
        boxVals = getBoxValues grid r c           -- Extracts all values in the 3x3 box containing (r, c)
        usedVals = nub (rowVals ++ colVals ++ boxVals)  -- Combines and removes duplicates from row, column, and box values
    in [1..9] \\ usedVals  -- Subtracts used values from the list [1..9], giving the possible values for cell (r, c)


    
    -- to create random number
generateRandom :: Int -> Int -> IO Int
generateRandom minVal maxVal = do
    time <- round <$> getPOSIXTime
    let randomNum = (time `mod` (maxVal - minVal + 1)) + minVal
    return randomNum

--  to print the grid
printGrid :: [[Int]] -> IO ()
printGrid = mapM_ print
