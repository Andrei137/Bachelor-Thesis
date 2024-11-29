(+^) :: Int -> Int -> Int -> Int
(+^) power x y = fastexp power (x + y)
(+^^) = (+^ 2)

fastexp :: Int -> Int -> Int
fastexp pow base 
    | pow == 0 = 1
    | even pow = half * half
    | odd pow = base * (fastexp (pow - 1) base)
    where
        half = fastexp (pow `div` 2) base
fastexp' = flip fastexp

range :: Int -> Int -> [Int]
range = flip replicate
-- range start count = [start..start + count]

sumOp :: Int -> Int -> Int
sumOp = (+)
-- sumOp = (+^^)

sumList :: [Int] -> Int
sumList = foldl sumOp 0 

-- Functors
listPow' :: Int -> Int -> Int -> [Int]
listPow' count base pow = fmap ($ pow) $ fmap fastexp' (range base count)

justListPow' :: Int -> Int -> Int -> Maybe [Int]
justListPow' count base pow = fmap (fmap ($ pow)) $ fmap (fmap fastexp') $ Just (listPow' count base 1)

rightJustListPow' :: Int -> Int -> Int -> Either String (Maybe [Int])
rightJustListPow' count base pow = fmap (fmap $ fmap ($ pow)) $ fmap (fmap $ fmap fastexp') $ Right (justListPow' count base 1)

sumListPow' :: Int -> Int -> Int -> Int
sumListPow' count base pow = sumList $ listPow' count base pow

sumJustListPow' :: Int -> Int -> Int -> Maybe Int
sumJustListPow' count base pow = fmap sumList (justListPow' count base pow)

sumRightJustListPow' :: Int -> Int -> Int -> Either String (Maybe Int)
sumRightJustListPow' count base pow = fmap (fmap sumList) (rightJustListPow' count base pow)

-- Applicative
listPow :: Int -> Int -> Int -> [Int]
listPow count base pow = fastexp <$> pure pow <*> (range base count)

justListPow :: Int -> Int -> Int -> Maybe [Int]
justListPow count base pow = fmap <$> (fastexp <$> pure pow) <*> Just (listPow count base 1)

rightJustListPow :: Int -> Int -> Int -> Either String (Maybe [Int])
rightJustListPow count base pow = fmap <$> (fmap <$> (fastexp <$> pure pow)) <*> Right (justListPow count base 1)

sumListPow :: Int -> Int -> Int -> Int
sumListPow = sumListPow'

sumJustListPow :: Int -> Int -> Int -> Maybe Int
sumJustListPow count base pow = sumList <$> (justListPow count base pow)

sumRightJustListPow :: Int -> Int -> Int -> Either String (Maybe Int)
sumRightJustListPow count base pow = (fmap sumList) <$> (rightJustListPow count base pow)
