module OperatorExample where

    addOne num = num + 1
    timesTwo num = num * 2
    squared num = num * num
    minusFive num = num - 5
    findResult num = minusFive (squared (timesTwo (addOne num)))
    -- ($) function application operator
    -- (.) function composition operator
    -- infix = function between args
    -- prefix = function and then args
