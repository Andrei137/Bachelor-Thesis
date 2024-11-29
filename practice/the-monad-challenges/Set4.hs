{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where
import MCPrelude
import Types

-- Generalizing State and Maybe
{-
    generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
    yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    (a -> b -> c) -> m a -> m b -> m c

    genTwo :: Gen a -> (a -> Gen b) -> Gen b
    link :: Maybe a -> (a -> Maybe b) -> Maybe b
    m a -> (a -> m b) -> m b
-}

-- Formalizing the Pattern
class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

-- Creating Instances
instance Monad Gen where
    a `bind` f = Gen $ \seed ->
        let (value, nextSeed) = (runGen a) seed
        in runGen (f value) nextSeed

    return a = Gen $ \seed -> (a, seed)

instance Monad Maybe where
    Nothing `bind` _ = Nothing
    (Just a) `bind` f = f a

    return a = Just a

instance Monad [] where
    xs `bind` f = foldr (\x xs -> f x ++ xs) [] xs

    return a = [a]

-- Revisiting Other Generic Functions
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b =
    a `bind` \a' ->
    b `bind` \b' ->
    return (f a' b')

sequence :: [Gen a] -> Gen [a]
sequence = foldr f (return [])
    where
        f a xs =
            a `bind` \a' ->
            xs `bind` \xs' ->
            return (a':xs')

(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
f =<< a =
    a `bind` \a' ->
    f a'

join :: Maybe (Maybe a) -> Maybe a
join a = a `bind` id

liftM3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 f a b c =
    a `bind` \a' ->
    b `bind` \b' ->
    c `bind` \c' ->
    return (f a' b' c')

ap :: [a -> b] -> [a] -> [b]
ap f a =
    f `bind` \f' ->
    a `bind` \a' ->
    return (f' a')
