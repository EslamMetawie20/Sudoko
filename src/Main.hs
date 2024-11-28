module Main where


import Utilities (printGrid)
import GameLogic (generateSudoku, removeNumbers, updateGrid)

-- بدء اللعبة مع المستخدم
playGame :: [[Int]] -> [[Int]] -> Int -> IO ()
playGame puzzle solution errors = do
    putStrLn "Current Sudoku Puzzle:"
    printGrid puzzle
    putStrLn "Enter row (0-8), column (0-8), and number (1-9) separated by spaces (or -1 to quit):"
    input <- getLine
    if input == "-1"
        then putStrLn "Game Over! You quit the game."
        else do
            let inputValues = words input
            if length inputValues /= 3
                then do
                    putStrLn "Invalid input! Please enter three numbers separated by spaces."
                    playGame puzzle solution errors
                else do
                    let [r, c, n] = map read inputValues :: [Int]
                    if r < 0 || r >= 9 || c < 0 || c >= 9 || n < 1 || n > 9
                        then do
                            putStrLn "Invalid values! Row and column must be between 0-8, and number between 1-9."
                            playGame puzzle solution errors
                        else if solution !! r !! c /= n
                            then do
                                let newErrors = errors + 1
                                putStrLn $ "Wrong! Errors: " ++ show newErrors ++ "/3"
                                if newErrors >= 3
                                    then putStrLn "Game Over! You exceeded the maximum errors."
                                    else playGame puzzle solution newErrors
                            else do
                                let newPuzzle = updateGrid r c n puzzle
                                if newPuzzle == solution
                                    then do
                                        putStrLn "Congratulations! You solved the Sudoku puzzle."
                                        printGrid newPuzzle
                                    else do
                                        putStrLn "Correct! Updated Puzzle:"
                                        playGame newPuzzle solution errors



main :: IO ()
main = do
    putStrLn "Choose difficulty: 1 (Easy), 2 (Medium), 3 (Hard):"
    difficulty <- readLn :: IO Int
    if difficulty `notElem` [1, 2, 3]
        then do
            putStrLn "Invalid choice! Please choose 1, 2, or 3."
            main
        else do
            let numRemove = case difficulty of
                    1 -> 20
                    2 -> 40
                    _ -> 60
            solution <- generateSudoku
            putStrLn "Puzzle will now be created."
            puzzle <- removeNumbers numRemove solution
            putStrLn "Puzzle ready. Start solving!"
            printGrid puzzle
            playGame puzzle solution 0

