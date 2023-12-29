module Main where

isDivisible :: Maybe Int -> Bool
isDivisible Nothing = False
isDivisible (Just n) = n /= 0

safeDiv :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv _ (Just 0) = Nothing
safeDiv _ Nothing = Nothing
safeDiv Nothing _ = Nothing
safeDiv (Just x) (Just y) = if isDivisible (Just y) then Just (x `div` y) else Nothing

safeSqrt :: Double -> Maybe Double
safeSqrt n
  | n < 0 = Nothing
  | otherwise = Just $ sqrt n

safeLog :: Double -> Maybe Double
safeLog n
  | n <= 0 = Nothing
  | otherwise = Just $ log n

main :: IO ()
main = do
  print (safeDiv (Just 10) (Just 2)) -- Should print: Just 5
  print (safeDiv (Just 10) Nothing) -- Should print: Nothing
  print (safeDiv Nothing (Just 2)) -- Should print: Nothing
  print (safeDiv (Just 10) (Just 0)) -- Should print: Nothing
  print (safeSqrt 4) -- Should print: Just 2.0
  print (safeSqrt (-4)) -- Should print: Nothing
  print (safeLog 1) -- Should print: Just 0.0
  print (safeLog 0) -- Should print: Nothing
  print (safeLog (-1)) -- Should print: Nothing