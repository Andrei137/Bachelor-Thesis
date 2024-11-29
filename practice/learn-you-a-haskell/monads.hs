import Control.Monad.Writer

maybe4 :: Maybe String
maybe5 :: Maybe String
list2 :: [(Int, Char)]

maybe1 = let x = 3; y="!" in Just (show x ++ y)
maybe2 = Just 3 >>= (\x -> Just (show x ++ "!"))
maybe3 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
maybe4 = Just 3 >>= (\x ->
            Just "!" >>= (\y ->
            Just (show x ++ y)))
maybe5 = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

list1 = [1,2] >>= \n -> "ab" >>= \ch -> return (n, ch)
list2 = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
