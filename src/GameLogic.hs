module GameLogic (solve, generateSudoku, removeNumbers, updateGrid) where

import Utilities (shuffle)
import Validation (isValid)

-- تحديث الشبكة بإدخال رقم جديد
updateGrid :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateGrid r c n grid = take r grid ++
                        [take c (grid !! r) ++ [n] ++ drop (c + 1) (grid !! r)] ++
                        drop (r + 1) grid

-- حل الشبكة باستخدام الـ Backtracking
solve :: [[Int]] -> Int -> Int -> IO (Maybe [[Int]])
solve grid r c
    | r == 9 = return $ Just grid -- انتهى الحل
    | c == 9 = solve grid (r + 1) 0 -- انتقل للصف التالي
    | grid !! r !! c /= 0 = solve grid r (c + 1) -- تخطي الخانات المملوءة
    | otherwise = tryNumbers grid r c [1..9]

tryNumbers :: [[Int]] -> Int -> Int -> [Int] -> IO (Maybe [[Int]])
tryNumbers grid r c [] = return Nothing -- فشل الحل
tryNumbers grid r c (n:ns)
    | isValid grid r c n = do
        let newGrid = updateGrid r c n grid
        result <- solve newGrid r (c + 1)
        case result of
            Just solvedGrid -> return $ Just solvedGrid
            Nothing -> tryNumbers grid r c ns
    | otherwise = tryNumbers grid r c ns

-- توليد شبكة سودوكو عشوائية
generateSudoku :: IO [[Int]]
generateSudoku = do
    let emptyGrid = replicate 9 (replicate 9 0)
    Just solution <- solve emptyGrid 0 0
    shuffleRows solution

-- إعادة ترتيب الصفوف
shuffleRows :: [[Int]] -> IO [[Int]]
shuffleRows grid = do
    indices <- shuffle [0..8]
    return [grid !! i | i <- indices]

-- إزالة أرقام بناءً على مستوى الصعوبة
removeNumbers :: Int -> [[Int]] -> IO [[Int]]
removeNumbers n grid = do
    positions <- generateUniquePositions n
    foldl (\acc (r, c) -> acc >>= (\g -> return $ updateGrid r c 0 g)) (return grid) positions

generateUniquePositions :: Int -> IO [(Int, Int)]
generateUniquePositions n = do
    allPositions <- shuffle [(r, c) | r <- [0..8], c <- [0..8]]
    return $ take n allPositions
