module FizzBuzz where
    fizzBuzz n
        | rem n 15 == 0 = "FizzBuzz"
        | rem n 3 == 0 = "fizz"
        | rem n 5 == 0 = "buzz"
        | otherwise = show n
    main = map fizzBuzz [1..100]