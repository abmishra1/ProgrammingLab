import Data.Array

type Value = Int
type Position = (Int, Int)
type Sudoku = Array Position Value

readSudoku :: String -> [[Integer]]
readSudoku input = read (input)


main = do
    putStrLn "Enter the sudoku in  a list of list format seperated by commas"
    input <- getLine 
    let sudoku = (createSudoku input)
    let solved = (solve sudoku)
    printSudoku solved

printSudoku :: Maybe Sudoku -> IO ()
printSudoku Nothing  = putStrLn "No solution"
printSudoku (Just b) = mapM_ putStrLn [([show $ b ! (row,col) | col<-[0..8]]) | row <- [0..8]]


solve :: Sudoku -> Maybe Sudoku
solve = headOrNothing . solutions

headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x


solveSudoku :: Sudoku -> [Sudoku]
solveSudoku sudoku = solveOnePos $( getEmptyPosition sudoku ) sudoku

createSudoku :: [[Value]] -> [(Position, Value)]
createSudoku  values = concatMap createRow $ (zip [0..8] values)
    where
        createRow :: (Int,[Value]) -> [((Int,Int),Value)]
        createRow (row, values) = createCol row $ zip [0..8] values

        createCol :: Int -> [(Int, Value)] -> [((Int,Int), Value)]
        createCol row cols = map (\(col,v) -> ((row,col),v)) cols

solveOnePos :: [Position] -> Sudoku -> [Sudoku]
solveOnePos [] sudoku = [sudoku]
solveOnePos (curPos:remPositions) soduko =
        -- substitute valid values at curPos
         concatMap (solveOnePos remPositions) validSudokus
    where
        validValues = filter (isValueValid sudoku curPos [1..9])
        validSudokus = map (updatePos sudoku curPos) validValues
    
updatePos :: Sudoku -> Position -> Value -> Sudoku
updatePos sudoku pos val = (sudoku // [pos, val])
        
isValueValid :: Sudoku -> Position -> Value -> Bool
isValueValid val pos@(row, col) sudoku =
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