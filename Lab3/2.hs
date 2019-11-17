import Data.List

substring :: String -> [String]
substring [] = []
substring xs = subs xs ++ substring (tail xs)
        where
            subs xs = foldl step [] xs
            step [] a = [[a]]
            step acc a = (head acc ++ [a]) : acc

sortList :: [String] -> [Int]
sortList x = map (\l -> length l) . group . sort $ [sort i | i <- x] 

count :: [Int] -> Int
count xs = foldl (\acc x -> acc + ((x * (x - 1)) `div` 2)) 0 xs

anagram :: (String, String) -> Int
anagram (s1, s2) = count . sortList $ substring $ (s1 ++ s2)
