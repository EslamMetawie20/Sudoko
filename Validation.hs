module Validation (validRow, validColumns, validBoxes, isValid) where

import Data.List (transpose, nub)

-- Check that the row does not contain duplicate elements
validRow :: [Int] -> Bool
validRow row = let nonZeroes = filter (/= 0) row
               in length nonZeroes == length (nub nonZeroes)

-- Validate all columns
validColumns :: [[Int]] -> Bool
validColumns = all validRow . transpose

-- Split the grid into 3x3 boxes
getBoxes :: [[Int]] -> [[Int]]
getBoxes grid = [concat [take 3 (drop (3 * col) row) | row <- take 3 (drop (3 * row) grid)]
                | row <- [0, 1, 2], col <- [0, 1, 2]]

-- Validate the 3x3 boxes
validBoxes :: [[Int]] -> Bool
validBoxes grid = all validRow (getBoxes grid)

-- Check if the number is valid for placement
isValid :: [[Int]] -> Int -> Int -> Int -> Bool
isValid grid r c n =
    let rowValid = n `notElem` (grid !! r)
        colValid = n `notElem` (transpose grid !! c)
        boxValid = n `notElem` getBoxes grid !! (r `div` 3 * 3 + c `div` 3)
    in rowValid && colValid && boxValid
