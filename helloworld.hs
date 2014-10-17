main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
