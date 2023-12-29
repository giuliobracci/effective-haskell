module PackageDelivery.Main where

import Control.Monad (forever, guard)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import System.IO (hFlush, stdout)

data Status = InTransit | Delivered deriving (Show, Eq)

data Package = Package {packageId :: String, status :: Status} deriving (Show)

type DeliverySystem = [Package]

updatePackageStatus :: Package -> Status -> Package
updatePackageStatus package newStatus = Package {packageId = packageId package, status = newStatus}

findPackage :: DeliverySystem -> String -> Maybe Package
findPackage system id = find (\p -> packageId p == id) system

updatePackageStatusInSystem :: DeliverySystem -> String -> Status -> DeliverySystem
updatePackageStatusInSystem system id newStatus = map updateIfMatch system
  where
    updateIfMatch package
      | packageId package == id = updatePackageStatus package newStatus
      | otherwise = package

deliverPackage :: DeliverySystem -> String -> Maybe DeliverySystem
deliverPackage system id = do
  package <- findPackage system id
  guard $ status package /= Delivered
  return $ updatePackageStatusInSystem system (packageId package) Delivered

loop :: DeliverySystem -> IO ()
loop system = do
  putStr "Enter package id: "
  hFlush stdout -- Make sure the prompt is printed immediately
  id <- getLine
  case deliverPackage system id of
    Nothing -> do
      putStrLn "Error: Could not deliver package."
      loop system
    Just newSystem -> do
      putStrLn "Package delivered successfully."
      print newSystem -- Print the new system state
      loop newSystem -- Recurse with the updated system state

main :: IO ()
main = loop []