import SudokusExample
import DiagonalSudokusExample
import MySudokuDiagonales
import MySudoku 
import Data.List.Split (splitOn)

main::IO()
main = do
    putStrLn "Welcome to Sudoku Solver!"
    putStrLn "1. Solve Regular Sudoku"
    putStrLn "2. Solve Diagonal Sudoku"
    putStrLn "3. Quit"
    putStr "Enter your choice: "
    choice <- getLine
    case choice of
        "1" -> solveSudoku False
        "2" -> solveSudoku True
        "3" -> putStrLn "Thank you for playing!"
        _   -> do
            putStrLn "Invalid choice. Please try again."
            main

--Seleccionar eejmplo o ingresar un sudoku
solveSudoku :: Bool -> IO ()
solveSudoku isDiagonal = do
    putStrLn $ "You chose to solve a " ++ (if isDiagonal then "diagonal" else "regular") ++ " Sudoku."
    putStrLn "1. Use an example Sudoku"
    putStrLn "2. Input your own Sudoku"
    putStr "Enter your choice: "
    choice <- getLine
    case choice of
        "1" -> solveSudokuExample isDiagonal
        "2" -> solveSudokuInput isDiagonal
        _   -> do
            putStrLn "Invalid choice. Please try again."
            solveSudoku isDiagonal





solveSudokuExample :: Bool -> IO ()
solveSudokuExample isDiagonal = do
    putStrLn "Choose difficulty:"
    putStrLn "1. Easy"
    putStrLn "2. gentle"
    putStrLn "3. diabolical"
    putStrLn "4. unsolvable"
    putStr "Enter your choice: "
    difficulty <- getLine
    let sudoku = if isDiagonal
                 then selectDiagonalSudoku difficulty
                 else selectRegularSudoku difficulty
    putStrLn "Selected Sudoku:"
    putStrLn (show sudoku)
    let solution = if isDiagonal
                   then MySudokuDiagonales.solve sudoku
                   else MySudoku.solve sudoku
    putStrLn "Solution:"
    putStrLn (show solution)
    main

solveSudokuInput :: Bool -> IO ()
solveSudokuInput isDiagonal = do
    putStrLn "Enter your Sudoku (use - for empty cells, separate rows with newlines):"
    putStrLn "example: [\"9--621-84\",\"1--759263\",\"276834-51\",\"369-7-41-\",\"4859-6372\",\"712-48695\",\"637---5-9\",\"521-97836\",\"--45---27\"]"
    input <- getLine
    let sudoku = read input :: [String]
    if (if isDiagonal then MySudokuDiagonales.validate else MySudoku.validate) sudoku
        then do
            putStrLn "Valid Sudoku. Solving..."
            let solution = if isDiagonal
                           then MySudokuDiagonales.solve sudoku
                           else MySudoku.solve sudoku
            putStrLn "Solution:"
            putStrLn (show solution)
        else putStrLn "Invalid Sudoku. Please try again."
    main




--selectRegularSudoku :: String -> Grid
selectRegularSudoku difficulty = 
    case difficulty of
        "1" -> SudokusExample.easy
        "2" -> SudokusExample.gentle
        "3" -> SudokusExample.diabolical
        "4" -> SudokusExample.unsolvable
        _   -> SudokusExample.easy  -- Default to easy if invalid input

--selectDiagonalSudoku :: String -> Grid
selectDiagonalSudoku difficulty = 
    case difficulty of
        "1" -> DiagonalSudokusExample.easy
        "2" -> DiagonalSudokusExample.gentle
        "3" -> DiagonalSudokusExample.diabolical
        "4" -> DiagonalSudokusExample.unsolvable
        _   -> DiagonalSudokusExample.easy  -- Default to easy if invalid input

--printSudoku::[String]
