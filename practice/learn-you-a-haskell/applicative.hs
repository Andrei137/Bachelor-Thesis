import Control.Applicative

gauss :: Int -> Maybe Int
gauss n = sum <$> (pure (++) <*> Just [1..half] <*> Just [half + 1, half + 2..n])
    where half = n `div` 2

factorial :: Int -> Either String Int
factorial 0 = Right 1
factorial n = (*) <$> pure n <*> factorial (n - 1)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs