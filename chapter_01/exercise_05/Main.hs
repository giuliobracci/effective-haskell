module Main where

    makeGreeting salutation person = 
        let salutationWithSpace = salutation <> " "
        in salutationWithSpace <> person
    extendedGreeting person =
        let hello = makeGreeting "Hello" person
            goodDay = makeGreeting "I hope you have a good day." person
            goodBye = makeGreeting "See you later. Bye bye" person
        in hello <> "\n" <> goodDay <> "\n" <> goodBye
    extendedGreeting' person =
        let joinWithNewlines a b = a <> "\n" <> b
            helloAndGoodbye hello goodbye =
                let hello' = makeGreeting hello person
                    goodbye' = makeGreeting goodbye person
                in joinWithNewlines hello' goodbye'
        in helloAndGoodbye "Hello" "Goodbye"

    main = putStrLn $ extendedGreeting' "Alessio"