{-
    To find the number of anagram substring pairs in a string. 
-}
import Data.List

{- Utility 1: Generate list of all substrings of a string
    Take a suffix and find prefixes of it, eg for "abcd":
    SUFFIX : ITS PREFIXES
    "abcd" : "abcd", "abc", "ab", "a"
    "bcd"  : "bcd", "bc", "b"
    "cd"   : "cd", "c"
    "d"    : "d"
-}
genSubstring :: String -> [String]
genSubstring "" = []
-- Recursion: Consider suffix starting from current index
-- then consider suffix starting at later positions
genSubstring xs = genPrefix xs ++ genSubstring (tail xs)
        where
            -- generate prefixes of a suffix by scanning left to right
            genPrefix xs = foldl accPrefix [] xs
            -- accPrefix accmulates the prefixes
            accPrefix [] a = [[a]]
            accPrefix acc a = ((head acc) ++ [a]) : acc

{-
    Utility 2:
        1. Sort individual strings and then sort all strings to bring anagrams together
        2. Now return count of members in each group
-}            
groupAnagrams :: [String] -> [Int]
groupAnagrams x = map (\l -> length l) . group . sort $ [sort i | i <- x] 

-- Utility 3: If there is a group of n anagrams, we get (n choose 2) pairs, then we sum these.
count :: [Int] -> Int
count xs = foldl (\acc x -> acc + ((x * (x - 1)) `div` 2)) 0 xs

-- Main function: Call utilities in order
anagram :: [String] -> Int
anagram (s1:s2:[]) = count . groupAnagrams $ genSubstring $ (s1 ++ s2)
anagram _ = error "Exactly two strings required"
