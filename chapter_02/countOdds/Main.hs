module Main where

countOdds numbers =
  length $ filter isOdd numbers
  where
    isOdd number = number `rem` 2 == 1

main = print $ countOdds [1 .. 10]