module Utilities (shuffle, generateRandom, printGrid) where

import Data.Time.Clock.POSIX (getPOSIXTime)

-- خلط قائمة
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    randIndex <- generateRandom 0 (length xs - 1)
    let (front, (x:back)) = splitAt randIndex xs
    rest <- shuffle (front ++ back)
    return (x:rest)

-- توليد رقم عشوائي باستخدام الوقت
generateRandom :: Int -> Int -> IO Int
generateRandom minVal maxVal = do
    time <- round <$> getPOSIXTime
    let randomNum = (time `mod` (maxVal - minVal + 1)) + minVal
    return randomNum

-- طباعة الشبكة
printGrid :: [[Int]] -> IO ()
printGrid = mapM_ print
