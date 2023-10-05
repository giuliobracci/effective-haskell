module Main where
    printSmallNumber num =
        let msg = if num < 10
            then show num
            else "the number is too big"
        in print msg
    guardSize num
        | num < 3 = "that's a small number"
        | num < 10 = "that's a medium number"
        | num < 100 = "that's a large number"
        | num < 1000 = "that's a very big number"
        | otherwise = "that's an incredible number"
    guardSize' num
        | num > 0 =
            let size = "positive"
            in exclaim size
        | num < 3 = exclaim "small"
        | num < 100 = exclaim "medium"
        | otherwise = exclaim "large"
        where
            exclaim message = "that's a " <> message <> " number!"
    main = print $ guardSize 300