module Main where

import Data.Char (toUpper)
import Data.Map (fromList)
import Data.Map qualified as Map (Map, map)
import Data.Maybe

add :: (Num a) => a -> a -> a
add x y = x + y

increment :: (Num a) => a -> a
increment x = x + 1

decrement :: (Num a) => a -> a
decrement x = x - 1

x :: Integer
x = 3

y :: Maybe Integer
y = Just 3

z :: Maybe Integer
z = y >>= \n -> return $ (decrement . decrement) n

z' :: Maybe Integer
z' = do fmap (increment . increment) y

maybeIncrement :: (Num a, Ord a) => a -> Maybe a
maybeIncrement x
  | x > 0 = Just (x + 1)
  | otherwise = Nothing

numbers :: [Maybe Integer]
numbers = [Just 1, Nothing, Just 2, Just 0, Just (-1), Nothing, Just 3]

incrementNumbers :: [Maybe Integer] -> [Maybe Integer]
incrementNumbers = map (>>= maybeIncrement)

maybeDouble :: (Num a, Ord a) => a -> Maybe a
maybeDouble x
  | x > 0 = Just (x * 2)
  | otherwise = Nothing

doubleNumbers :: [Maybe Integer] -> [Maybe Integer]
doubleNumbers = map (>>= maybeDouble)

userData :: Map.Map String (Maybe String)
userData = fromList [("username", Just "johnDoe"), ("email", Nothing), ("city", Just "london")]

maybeCapitalize :: String -> Maybe String
maybeCapitalize "" = Nothing
maybeCapitalize str = Just (map toUpper str)

capitalizeUserData :: Map.Map String (Maybe String) -> Map.Map String (Maybe String)
capitalizeUserData userData =
  userData

main :: IO ()
main = undefined