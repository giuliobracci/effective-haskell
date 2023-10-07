module Main where
    countdown n = 
        if n < 0 then []
        else n : countdown (n - 1)
    factors num =
        factors' num 2
        where
            factors' num factor
                | num == 1 = []
                | (num `rem` factor) == 0 = factor : factors' (num `div` factor) factor
                | otherwise = factors' num (factor + 1)

    reduce fn acc list =
        if null list then acc
        else
            let intermediateValue = fn acc (head list)
            in reduce fn intermediateValue (tail list)
    isBalanced str =
        reduce checkParens 0 str == 0
        where
            checkParens count letter
                | letter == '(' = count + 1
                | letter == ')' = count - 1
                | otherwise = count

    main = print $ countdown 10