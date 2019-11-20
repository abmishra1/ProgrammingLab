{-
Steps: 
        1. Map the sequence to obtain a list of f values
        2. Find the leftmost maximum's index "iy"
        3. Return the element in this "iy" index in input seq
        Note since seq can be empty, we have returned "Maybe a" type
-}
greatest :: (a -> Int) -> [a] -> Maybe a
greatest f [] = Nothing
greatest f seq = do
        let y = map f seq
        let iy = head $ filter ((== maximum y) . (y !!)) [0..]
        Just (seq !! iy)