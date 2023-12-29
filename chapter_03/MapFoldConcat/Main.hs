module MapFoldConcat.Main where

import Text.Read.Lex (Number)

-- Map Exercise:
-- Write a function doubleAll that takes a list of integers
-- and returns a new list with all values doubled.

doubleAll :: [Int] -> [Int]
doubleAll = map (* 2)

sumAll :: [Int] -> Int
sumAll = sum

expandList :: [Int] -> [Int]
expandList = concatMap replicateSelf
  where
    replicateSelf :: Int -> [Int]
    replicateSelf n = replicate n n

sumOfSquares :: [Int] -> Int
sumOfSquares = sumAll . squareAll

customProduct :: [Int] -> Int
customProduct = foldl multiplyUntilZero 1
  where
    multiplyUntilZero acc n
      | n == 0 = 0
      | otherwise = acc * n

removeOdd :: [Int] -> [Int]
removeOdd = filter even

average :: [Double] -> Double
average list = sum list / fromIntegral (length list) -- Your implementation here

duplicateEach :: [Int] -> [Int]
duplicateEach = concatMap duplicate
  where
    duplicate n = [n, n]

countZeros :: [Int] -> Int
countZeros = foldl fn 0
  where
    fn acc 0 = acc + 1
    fn acc n = acc

incrementAll :: [Int] -> [Int]
incrementAll = map (+ 1) -- Your implementation here

squareAll :: [Int] -> [Int]
squareAll = map square
  where
    square n = n * n

removeEven :: [Int] -> [Int]
removeEven = filter odd

productAll :: [Int] -> Int
productAll = product

tripleEach :: [Int] -> [Int]
tripleEach = concatMap triple
  where
    triple n = [n, n, n]

countNegatives :: [Int] -> Int
countNegatives = foldl countNegatives' 0
  where
    countNegatives' acc n
      | n < 0 = acc + 1
      | otherwise = acc