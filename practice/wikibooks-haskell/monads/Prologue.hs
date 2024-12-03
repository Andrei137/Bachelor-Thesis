import Text.Read

retry msg f = do
    putStrLn msg *> f

{- Note: not much is gained by making use of Functors -}
interactiveDoubling' :: IO ()
interactiveDoubling' = do
    putStrLn "Choose a number"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Nothing -> retry "This is not a valid number... Retrying..." interactiveDoubling'
        Just x -> putStrLn $ "The double of your number is " ++ show (2 * x)

interactiveDoubling :: IO ()
interactiveDoubling = do
    putStrLn "Choose a number"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case fmap (2*) mx of
        Nothing -> retry "This is not a valid number... Retrying..." interactiveDoubling
        Just d -> putStrLn $ "The double of your number is " ++ show d

{- Note: a lot of clutter was removed by using Applicative -}
interactiveSumming' :: IO ()
interactiveSumming' = do
    putStrLn "Choose two numbers"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy :: Maybe Double
    case mx of
        Just x -> case my of
            Just y -> putStrLn $ "The sum of your numbers is " ++ show (x + y)
            Nothing -> retry "Invalid number... Retrying..." interactiveSumming'
        Nothing -> retry "Invalid number... Retrying..." interactiveSumming'

interactiveSumming :: IO ()
interactiveSumming = do
    putStrLn "Choose two numbers"
    mx <- readMaybe <$> getLine
    my <- readMaybe <$> getLine
    case (+) <$> mx <*> my of
        Just z -> putStrLn $ "The sum of your numbers is " ++ show z
        Nothing -> retry "Invalid number... Retrying..." interactiveSumming

interactiveConcatenating' :: IO ()
interactiveConcatenating' = do
    putStrLn "Choose two strings"
    sx <- getLine
    sy <- getLine
    putStrLn "Let's concatenate them!"
    putStrLn $ sx ++ sy

interactiveConcatenating :: IO ()
interactiveConcatenating = do
    sz <- putStrLn "Choose two strings" *> ((++) <$> getLine <*> getLine)
    putStrLn "Let's concatenate them!" *> putStrLn sz
