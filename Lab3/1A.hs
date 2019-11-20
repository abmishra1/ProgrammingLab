{-
    Steps: 
    1. Obtain sum of of each list in the input list of lists
    2. Return product of these sums
-}
m :: [[Integer]] -> Integer
m x
    | length x == 0 = 0
    | otherwise = product (map sum x)
