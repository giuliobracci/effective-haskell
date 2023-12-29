module Main where

import Data.Maybe (isJust, isNothing)

data Status = InLibrary | Borrowed deriving (Eq, Show)

data Book = Book {title :: String, status :: Status} deriving (Show)

type Library = [Book]

type Result = Either String Library

isSameBook :: String -> Book -> Bool
isSameBook titleToSearch book = title book == titleToSearch

findBook :: String -> Library -> Maybe Book
findBook titleToSearch =
  foldr (\book acc -> if isSameBook titleToSearch book then Just book else acc) Nothing

addBook :: String -> Library -> Result
addBook bookTitle library =
  if isJust $ findBook bookTitle library
    then Left "Book already exists"
    else Right (Book {title = bookTitle, status = InLibrary} : library)

borrowBook :: String -> Library -> Result
borrowBook bookTitle =
  let borrow book = if isSameBook bookTitle book then book {status = Borrowed} else book
   in \library ->
        if isNothing $ findBook bookTitle library
          then Left "Book does not exist"
          else Right (map borrow library)

returnBook :: String -> Library -> Result
returnBook bookTitle =
  let returnToLibrary book = if isSameBook bookTitle book then book {status = InLibrary} else book
   in \library ->
        if isNothing $ findBook bookTitle library
          then Left "Book does not exist"
          else Right (map returnToLibrary library)

main :: IO ()
main = do
  let library = Right []
  let library' = library >>= addBook "Haskell Programming" >>= addBook "Haskell Teaching" >>= addBook "Haskell For Dummies"
  let library'' = library' >>= borrowBook "Haskell Programming"
  let library''' = library'' >>= borrowBook "Haskell Teaching"
  print library''' -- Should print: Right [Book {title = "Haskell For Dummies", status = InLibrary},Book {title = "Haskell Teaching", status = Borrowed},Book {title = "Haskell Programming", status = Borrowed}]