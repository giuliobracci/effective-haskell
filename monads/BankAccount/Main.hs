module BankAccount.Main where

newtype Account = Account
  { balance :: Double
  }
  deriving (Show)

type Result = Either String Account

createAccount :: Double -> Result
createAccount amount =
  if amount > 0
    then Right Account {balance = amount}
    else Left "To initialize an account you must deposit money"

deposit :: Double -> Account -> Result
deposit amount account
  | amount <= 0 = Left "To deposit you must have a valid amount"
  | otherwise = Right Account {balance = newBalance}
  where
    currentBalance = balance account
    newBalance = currentBalance + amount

withdraw :: Double -> Account -> Result
withdraw amount account
  | amount <= 0 = Left "To withdraw you must have a valid amount"
  | amount > currentBalance = Left "Insufficient funds"
  | otherwise = Right Account {balance = newBalance}
  where
    currentBalance = balance account
    newBalance = currentBalance - amount

main :: IO ()
main = do
  let account = createAccount 100
  print account -- Should print: Right (Account {balance = 100.0})
  let account' = account >>= deposit 50
  print account' -- Should print: Right (Account {balance = 150.0})
  let account'' = account' >>= withdraw 200
  print account'' -- Should print: Left "Insufficient funds"