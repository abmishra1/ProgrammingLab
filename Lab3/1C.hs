-- Implemented a custom data type "List a", defined type conversion functions for it

-- Define custom data type recursively, base case empty and induction step
data List a = Empty | Cons a (List a) deriving (Show)  

-- Construct my List by recurively popping head from input list
toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = x `Cons` toList xs

-- Construct Haskell list by recursively popping head from my List
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x xs) = x : toHaskellList xs