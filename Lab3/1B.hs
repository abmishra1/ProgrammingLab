greatest :: (a -> Int) -> [a] -> a
greatest f seq = do
        let y = map f seq
        let iy = head $ filter ((== maximum y) . (y !!)) [0..]
        -- let iy = (maximum y) elemIndex y
        seq !! iy