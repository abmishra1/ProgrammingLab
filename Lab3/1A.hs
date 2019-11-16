m :: [[Integer]] -> Integer
m x = do
    let y = map sum x
    product y

