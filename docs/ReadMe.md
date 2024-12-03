# Sudoku Game in Haskell


## Developers
- Developed by: `Abdel Rahman Abu Baker` und `Eslam Metawie`
---

## Description
A Sudoku game built in Haskell that allows users to play Sudoku puzzles of varying difficulty. The project includes features such as automatic puzzle generation, error tracking, and interactive gameplay. This project was developed as part of a university course.

---

## Requirements
- GHC (Glasgow Haskell Compiler) installed
---

## Difficulty Levels
- Easy: 20 numbers removed
- Medium: 40 numbers removed
- Hard: 60 numbers removed
---

## Project Structure
- Main.hs: Entry point of the application, handles user interaction.
- GameLogic.hs: Core logic for solving and generating Sudoku puzzles.
- Utilities.hs: Helper functions such as shuffling and random number generation.
- Validation.hs: Validations for grid rows, columns, and sub-grids.
----
## How to Play

1. **Select a difficulty level:** Easy, Medium, or Hard.
2. **Generate a Sudoku puzzle:** A puzzle will be generated with missing numbers based on the chosen difficulty.
3. **Enter your guesses:** Provide your input in the format:
    - for Example: `1 2 5` will place the number `5` in **row 1, column 2**.
4. **Error limit:** You are allowed a maximum of **3 errors**. If you exceed this limit, the game will end.
5. **Win condition:** Solve the puzzle correctly to win!
