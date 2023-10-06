module Main where
    -- get list indexes -> !! index
    -- merge lists -> <>
    list = 1 : [2, 3]
    list' = 1 : 2 : [3, 4, 5]
    -- head function gets the head of the list
    -- tail function gets the tail of the list
    -- both can cause exceptions when called on an empty list
    listHead = head list
    listTail = tail list
    main = print $ words !! 2
        where
            words = ["foo", "bar", "baz", "boo"]
    -- prepend to list (:) operator
