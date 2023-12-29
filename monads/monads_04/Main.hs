module Main where

safeDiv :: Double -> Double -> Either String Double
safeDiv _ 0 = Left "Error, division by zero"
safeDiv x y = Right $ x / y

safeSqrt :: Double -> Either String Double
safeSqrt x
  | x < 0 = Left "Cannot apply sqrt to negative number"
  | otherwise = Right $ sqrt x

main :: IO ()
main = do
  print (safeDiv 10 2) -- Should print: Right 5.0
  print (safeDiv 10 0) -- Should print: Left "Division by zero error"
  print (safeSqrt 4) -- Should print: Right 2.0
  print (safeSqrt (-4)) -- Should print: Left "Square root of negative number error"