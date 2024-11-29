{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Initial.Set2 where
import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    -- show :: Maybe a -> String
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
    -- (==) :: Maybe a -> Maybe a -> Bool
    Nothing == Nothing = True
    Just a == Just b = a == b
    _ == _ = False

-- Build a library of things that can fail
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((key, value):xs)
    | a == key = Just value
    | otherwise = lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b
    | b == 0 = Nothing
    | otherwise = Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = case maximumMay xs of
    Nothing -> Just x
    Just maxTail -> Just (max x maxTail)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = case minimumMay xs of
    Nothing -> Just x
    Just minTail -> Just (min x minTail)

-- Chains of Failing Computations
queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekMap string = case lookupMay string greekMap of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
        Nothing -> Nothing
        Just xsTail -> case maximumMay xsTail of
            Nothing -> Nothing
            Just dividend -> case headMay xs of
                Nothing -> Nothing
                Just divisor -> divMay (fromIntegral dividend) (fromIntegral divisor)

verifyQuery :: (GreekData -> String -> Maybe Double) -> Bool
verifyQuery f =
    f greekDataA "alpha" == Just 2.0 &&
    f greekDataA "beta" == Nothing &&
    f greekDataA "gamma" == Just 3.3333333333333335 &&
    f greekDataA "delta" == Nothing &&
    f greekDataA "zeta" == Nothing &&
    f greekDataB "rho" == Nothing &&
    f greekDataB "phi" == Just 0.24528301886792453 &&
    f greekDataB "chi" == Just 9.095238095238095 &&
    f greekDataB "psi" == Nothing &&
    f greekDataB "omega" == Just 24.0

verifyQueryGreek :: Bool
verifyQueryGreek = verifyQuery queryGreek

-- Generalizing chains of failures
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekMap string =
    lookupMay string greekMap `link` \xs ->
    tailMay xs `link` \xsTail ->
    maximumMay xsTail `link` \dividend ->
    headMay xs `link` \divisor ->
    divMay (fromIntegral dividend) (fromIntegral divisor)

verifyQueryGreek2 :: Bool
verifyQueryGreek2 = verifyQuery queryGreek2

-- Chaining variations
addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salariesMap name1 name2 =
    lookupMay name1 salariesMap `link` \salary1 ->
    lookupMay name2 salariesMap `link` \salary2 ->
    mkMaybe (salary1 + salary2)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f a b =
    a `link` \a' ->
    b `link` \b' ->
    mkMaybe (f a' b')

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salariesMap name1 name2 =
    yLink (\s1 s2 -> s1 + s2) salary1 salary2
    where
        salary1 = lookupMay name1 salariesMap
        salary2 = lookupMay name2 salariesMap

mkMaybe :: a -> Maybe a
mkMaybe = Just

-- Tailprod
tailProd :: Num a => [a] -> Maybe a
tailProd xs = transMaybe product (tailMay xs)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = transMaybe sum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = combine $ transMaybe maximumMay (tailMay xs)

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine $ transMaybe minimumMay (tailMay xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f xs =
    xs `link` \xs' ->
    mkMaybe $ f xs'

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just a) = a
