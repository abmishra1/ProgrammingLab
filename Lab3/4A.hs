{-
Importing modules and defining data types
Value : Entry in Sudokou ranges from 0 to 4 where 0 means empty
Position : Grid position maintained by (row,col)
Sudoku : Datatype for sudoku with each element accessed by (row,col) 
-}

import Data.Array

type Value = Int
type Position = (Int, Int)
type Sudoku = Array Position Value

{-
This is the main function it takes in input the sudoku as form of list of lists.
The input is then validtaed by checking if no of rows and column are equal to grid,
and if the input value is in range of grid as well as if there is some repetition
After performing the checks it call the solve function which solves the sudoku if
possible or else return "Nothing"
The solved sudoku is then printed by passing it to the printSudoku Function
-}
sudokuSolver :: [[Value]] -> IO()
sudokuSolver values =
    if (checkNoOfRows values) && (checkAllValues values) then
        do 
        let sudoku = array((0,0),(3,3)) $ (createSudoku values)
        let pos = range((0,0),(3,3))
        if checkRepeat sudoku  pos then
            do
            let solved = (solve sudoku)
            printSudoku solved
        else
            putStrLn "Repetition in given values"
    else
        putStrLn "Invalid Matrix given as input"

{-
This function checks if the input list of list is of 4 rows
-}
checkNoOfRows :: [[Value]] -> Bool
checkNoOfRows x = (length x == 4)        


{-
This function checks if the input sudoku value contains repetition by checking the
corresponding row, column and grid for duplicates
-}
checkRepeat :: Sudoku ->[Position]-> Bool
checkRepeat sudoku [] = True
checkRepeat sudoku (pos:xs)
        | (sudoku ! pos == 0) = checkRepeat sudoku xs
        | otherwise = do
                    let val = sudoku ! pos
                    let sudokuTemp = sudoku // [(pos,0)]
                    if (isValueValid sudokuTemp pos val) then 
                        checkRepeat sudoku xs
                    else
                        False

{-
This function checks if all the rows contains 4 columns each row is valid
-}
checkAllValues :: [[Value]] -> Bool
checkAllValues [] = True
checkAllValues (x:xs)
            | (length x == 4 && checkRowValues x) = checkAllValues xs
            | otherwise = False

{-
This function checks the row to validate each element present is between 0 to 4
-}
checkRowValues :: [Value] -> Bool
checkRowValues [] = True
checkRowValues (x:xs)
            | (x>=0 && x<=4 ) = checkRowValues xs
            | otherwise = False

{-
This function takes in Maybe Sudoku as input and outputs nothing if no solution is possible
or if the sudoku is valid outputs the sudoku
-}
printSudoku :: Maybe Sudoku -> IO ()
printSudoku Nothing  = putStrLn "No solution"
printSudoku (Just b) = mapM_ putStrLn [ show $ [ b ! pos | pos <- range ((row,0),(row,3))]  | row <- [0..3]]

{-
This function takes in input the given sudkou and gives out either "Nothing" if it is unsolvable
else otputs a possible answer 
-}
solve :: Sudoku -> Maybe Sudoku
solve = headOrNothing . solveSudoku

{-
This is a helper function which return Nothing if  empty or one of the possible solution
-}
headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

{-
This is a wrapper function over the solver which call the solver with giving the sudoku and the
empty locations as input
-}
solveSudoku :: Sudoku -> [Sudoku]
solveSudoku sudoku = solveOnePos (getEmptyPosition sudoku) sudoku


{-
This function takes in the list of list of numbers and change it into a sudoku format by assisgning
tuple of (Position,Value) by creating each cell from (0,0) to (3,3)
-}
createSudoku :: [[Value]] -> [(Position, Value)]
createSudoku  values = concatMap createRow $ (zip [0..3] values)
    where
        createRow :: (Int,[Value]) -> [((Int,Int),Value)]
        createRow (row, values) = createCol row $ zip [0..3] values

        createCol :: Int -> [(Int, Value)] -> [((Int,Int), Value)]
        createCol row cols = map (\(col,v) -> ((row,col),v)) cols

{-
This is the main solver function it takes input the partially filled sudoku and the positions which has 
to be filled it tries each possible value by checking for invalidation and doing backtracking and saves
the possible solutions to return
-}
solveOnePos :: [Position] -> Sudoku -> [Sudoku]
solveOnePos [] sudoku = [sudoku]
solveOnePos (curPos:remPositions) sudoku =
        -- substitute valid values at curPos
         concatMap (solveOnePos remPositions) validSudokus
    where
        validValues = filter (isValueValid sudoku curPos) [1..4]
        validSudokus = map (updatePos sudoku curPos) validValues

{-
This function updates the sudoku at the given position to the provided value
-}
updatePos :: Sudoku -> Position -> Value -> Sudoku
updatePos sudoku pos val = (sudoku // [(pos, val)])
    
{-
This function cheks if the given value can be wriiten at the provided position
by checking if there is no repetition in row,col or the grid
-}
isValueValid :: Sudoku -> Position -> Value -> Bool
isValueValid sudoku pos@(row, col) val  =
    canBeUsedInRow sudoku row val
    && canBeUsedInCol sudoku col val
    && canBeUsedInGrid sudoku pos val
 
{-
This function takes in the row number and value and checks if the value already
exists in the row or not.
-}   
canBeUsedInRow :: Sudoku -> Int -> Value -> Bool
canBeUsedInRow sudoku row val =
    not (elem val usedValues)
    where usedValues = [ sudoku ! pos | pos <- range((row,0),(row,3))]

{-
This function takes in the column number and value and checks if the value already
exists in the column or not.
-} 
canBeUsedInCol :: Sudoku -> Int -> Value -> Bool
canBeUsedInCol sudoku col val = 
    not (elem val usedValues)
    where usedValues = [ sudoku ! pos | pos <- range((0,col),(3,col))]

{-
This function takes in the position and value and checks if the value already
exists in the grid or not.
-}
canBeUsedInGrid :: Sudoku -> Position -> Value -> Bool
canBeUsedInGrid sudoku (row,col) val = 
    not ( elem val usedValues)
    where 
        usedValues = [ sudoku ! pos | pos <- range((r0,c0),(r0+1,c0+1))] where
        r0 = (row `div` 2)*2
        c0 = (col `div` 2)*2

{-
This function returns all the empty position in the given sudoku
-}
getEmptyPosition :: Sudoku -> [Position]
getEmptyPosition sudoku = [pos | pos <- range((0,0),(3,3)) , sudoku ! pos == 0]
