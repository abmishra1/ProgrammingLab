data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = x `Cons` toList xs

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x xs) = x : toHaskellList xs