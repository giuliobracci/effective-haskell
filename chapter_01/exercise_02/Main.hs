module Main where
    makeGreeting = \salutation person -> salutation <> " " <> person
    enthusiasticGreeting salutation = makeGreeting (salutation <> "!")
    greetPerson = makeGreeting "Hello"
    greetGeorge = flip makeGreeting "George"

    main = print "no salutation to show yet"
