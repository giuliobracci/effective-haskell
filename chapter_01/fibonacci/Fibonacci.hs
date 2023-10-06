module Fibonacci where
    fibonacci n
        | n <= 1     = n
        | otherwise  = fibonacci(n - 1) + fibonacci(n - 2)