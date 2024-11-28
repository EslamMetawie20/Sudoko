module Validation (validRow, validColumns, validBoxes, isValid) where

import Data.List (transpose, nub)

-- التحقق من أن الصف لا يحتوي على عناصر مكررة
validRow :: [Int] -> Bool
validRow row = let nonZeroes = filter (/= 0) row
               in length nonZeroes == length (nub nonZeroes)

-- التحقق من صحة الأعمدة
validColumns :: [[Int]] -> Bool
validColumns = all validRow . transpose

-- تقسيم الشبكة إلى مربعات صغيرة (3x3)
getBoxes :: [[Int]] -> [[Int]]
getBoxes grid = [concat [take 3 (drop (3 * col) row) | row <- take 3 (drop (3 * row) grid)] 
                | row <- [0, 1, 2], col <- [0, 1, 2]]

-- التحقق من صحة المربعات الصغيرة (3x3)
validBoxes :: [[Int]] -> Bool
validBoxes grid = all validRow (getBoxes grid)

-- تحقق إذا كان الرقم صالحًا للإضافة
isValid :: [[Int]] -> Int -> Int -> Int -> Bool
isValid grid r c n =
    let rowValid = n `notElem` (grid !! r)
        colValid = n `notElem` (transpose grid !! c)
        boxValid = n `notElem` getBoxes grid !! (r `div` 3 * 3 + c `div` 3)
    in rowValid && colValid && boxValid
