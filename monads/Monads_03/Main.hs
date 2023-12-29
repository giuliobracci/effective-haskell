module Main where

safeHead :: [a] -> Maybe a
safeHead (x : xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x : xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast list = Just (last list)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] index = Nothing
safeIndex list index
  | index < 0 = Nothing
  | index >= length list = Nothing
  | otherwise = Just (list !! index)

main :: IO ()
main = do
  print (safeHead [1, 2, 3]) -- Should print: Just 1
  print (safeHead ([] :: [Int])) -- Should print: Nothing
  print (safeTail [1, 2, 3]) -- Should print: Just [2,3]
  print (safeTail ([] :: [Int])) -- Should print: Nothing
  print (safeLast [1, 2, 3]) -- Should print: Just 3
  print (safeLast ([] :: [Int])) -- Should print: Nothing
  print (safeIndex [1, 2, 3] 1) -- Should print: Just 2
  print (safeIndex [1, 2, 3] 3) -- Should print: Nothing