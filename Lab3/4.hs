import Data.Array

type Value = Int
type Position = (Int, Int)
type Sudoku = Array Position Value

-- readRow :: String -> [Value]
-- readRow input = read ("[" ++ input ++ "]")

-- readSudoku :: Int -> [[Value]]
-- readSudoku 0 = [[]]
-- readSudoku n = do
--         rowVal <- getLine
--         let row = (readRow rowVal)
--         [row] : (readSudoku (n-1))

-- main = do
--     putStrLn "Enter the sudoku in  a grid format seperated by commas"
--     let input = readSudoku 9
--     let sudoku = array((0,0),(8,8)) $ (createSudoku input)
--     let solved = (solve sudoku)
--     printSudoku solved

wrapper :: [[Value]] -> IO()
wrapper values =
    if checkAllValues values then
        do 
        let sudoku = array((0,0),(8,8)) $ (createSudoku values)
        let pos = range((0,0),(8,8))
        if checkRepeat sudoku  pos then
            do
            let solved = (solve sudoku)
            printSudoku solved
        else
            putStrLn "Repetition in given values"
    else
        putStrLn "Invalid Value present"

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

checkAllValues :: [[Value]] -> Bool
checkAllValues [] = True
checkAllValues (x:xs)
            | (checkRowValues x) = checkAllValues xs
            | otherwise = False

checkRowValues :: [Value] -> Bool
checkRowValues [] = True
checkRowValues (x:xs)
            | (x>=0 && x<=9 ) = checkRowValues xs
            | otherwise = False

printSudoku :: Maybe Sudoku -> IO ()
printSudoku Nothing  = putStrLn "No solution"
printSudoku (Just b) = mapM_ putStrLn [ show $ [ b ! pos | pos <- range ((row,0),(row,8))]  | row <- [0..8]]


solve :: Sudoku -> Maybe Sudoku
solve = headOrNothing . solveSudoku

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x


solveSudoku :: Sudoku -> [Sudoku]
solveSudoku sudoku = solveOnePos (getEmptyPosition sudoku) sudoku

createSudoku :: [[Value]] -> [(Position, Value)]
createSudoku  values = concatMap createRow $ (zip [0..8] values)
    where
        createRow :: (Int,[Value]) -> [((Int,Int),Value)]
        createRow (row, values) = createCol row $ zip [0..8] values

        createCol :: Int -> [(Int, Value)] -> [((Int,Int), Value)]
        createCol row cols = map (\(col,v) -> ((row,col),v)) cols

solveOnePos :: [Position] -> Sudoku -> [Sudoku]
solveOnePos [] sudoku = [sudoku]
solveOnePos (curPos:remPositions) sudoku =
        -- substitute valid values at curPos
         concatMap (solveOnePos remPositions) validSudokus
    where
        validValues = filter (isValueValid sudoku curPos) [1..9]
        validSudokus = map (updatePos sudoku curPos) validValues
    
updatePos :: Sudoku -> Position -> Value -> Sudoku
updatePos sudoku pos val = (sudoku // [(pos, val)])
        
isValueValid :: Sudoku -> Position -> Value -> Bool
isValueValid sudoku pos@(row, col) val  =
    canBeUsedInRow sudoku row val
    && canBeUsedInCol sudoku col val
    && canBeUsedInGrid sudoku pos val
    
canBeUsedInRow :: Sudoku -> Int -> Value -> Bool
canBeUsedInRow sudoku row val =
    not (elem val usedValues)
    where usedValues = [ sudoku ! pos | pos <- range((row,0),(row,8))]


canBeUsedInCol :: Sudoku -> Int -> Value -> Bool
canBeUsedInCol sudoku col val = 
    not (elem val usedValues)
    where usedValues = [ sudoku ! pos | pos <- range((0,col),(8,col))]

canBeUsedInGrid :: Sudoku -> Position -> Value -> Bool
canBeUsedInGrid sudoku (row,col) val = 
    not ( elem val usedValues)
    where 
        usedValues = [ sudoku ! pos | pos <- range((r0,c0),(r0+2,c0+2))] where
        r0 = (row `div` 3)*3
        c0 = (col `div` 3)*3

getEmptyPosition :: Sudoku -> [Position]
getEmptyPosition sudoku = [pos | pos <- range((0,0),(8,8)) , sudoku ! pos == 0]
