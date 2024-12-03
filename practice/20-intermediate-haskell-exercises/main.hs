class Fluffy f where
    furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
    furry f = foldr (\a acc -> f a : acc) []

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
    furry _ Nothing = Nothing
    furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
    furry f g = \x -> f $ g x

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
    furry _ (EitherLeft (Right b)) = EitherLeft $ Right b
    furry f (EitherLeft (Left a)) = EitherLeft . Left $ f a

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
    furry _ (EitherRight (Left b)) = EitherRight $ Left b
    furry f (EitherRight (Right a)) = EitherRight . Right $ f a

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a
    -- Exercise 6
    -- Relative Difficulty: 3
    -- (use banana and/or unicorn)
    furry' :: (a -> b) -> m a -> m b
    furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
    banana f = foldr (\a acc -> f a ++ acc) []
    unicorn a = [a]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
    banana _ Nothing = Nothing
    banana f (Just a) = f a
    unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
    banana f g = \x -> f (g x) x
    unicorn f = \_ -> f

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
    banana _ (EitherLeft (Right b)) = EitherLeft $ Right b
    banana f (EitherLeft (Left a)) = f a
    unicorn a = EitherLeft (Left a)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
    banana _ (EitherRight (Left b)) = EitherRight $ Left b
    banana f (EitherRight (Right a)) = f a
    unicorn a = EitherRight (Right a)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma = banana (\f -> furry' f ma)

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = foldr (\x acc -> banana (\y -> furry' (y:) acc) (f x)) (unicorn []) xs

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb $ furry' (\a -> f a) ma
-- banana2 f ma mb = banana (\a -> furry' (f a) mb) ma

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc $ banana2 f ma mb

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md $ banana3 f ma mb mc

newtype State s a = State {
    state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
    furry f sa = State $ \s ->
        let (s', a) = state sa s
        in (s', f a)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
    banana f sa = State $ \s ->
        let (s', a) = state sa s
        in state (f a) s'
    unicorn a = State $ \s -> (s, a)
