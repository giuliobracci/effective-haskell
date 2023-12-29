module Main where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

sumList :: [Int] -> Int
sumList [] = 0
sumList (n : numbers) = n + sumList numbers

main = print $ sumList [1 .. 20]
