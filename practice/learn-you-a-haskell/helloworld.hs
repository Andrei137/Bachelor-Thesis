test :: IO ()
test = putStrLn "text" >> getLine >>= putStrLn

main = do
    putStrLn "Hello, what's your name?" :: IO ()
    name <- getLine :: IO String
    putStrLn ("Hey " ++ name ++ "! Nice to meet you!") :: IO ()
