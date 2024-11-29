{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where
import MCPrelude
import Types

-- Do Notation
{-
    rule1 :: m b
    calcFoo :: m a
    bar :: a -> m b
    myBar :: a -> b

    rule1 = do
        foo <- calcFoo
        bar foo

    rule1 = bind calcFoo (\foo -> bar foo)

    rul2 = do
        foo <- calcFoo
        return (myBar foo)
-}

-- Do Notation – operators
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

instance Monad Gen where
    a >>= f = Gen $ \seed ->
        let (value, nextSeed) = (runGen a) seed
        in runGen (f value) nextSeed

    return a = Gen $ \seed -> (a, seed)

instance Monad Maybe where
    Nothing >>= f = Nothing
    (Just a) >>= f = f a

    return = Just

instance Monad [] where
    xs >>= f = foldr (\x xs -> f x ++ xs) [] xs

    return a = [a]

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b =
    a >>= \a' ->
    b >>= \b' ->
    return (f a' b')

sequence :: [Gen a] -> Gen [a]
sequence = foldr f (return [])
    where
        f a xs =
            a >>= \a' ->
            xs >>= \xs' ->
            return (a':xs')

(=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
f =<< a =
    a >>= \a' ->
    f a'

join :: Maybe (Maybe a) -> Maybe a
join a = a >>= id

liftM3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 f a b c =
    a >>= \a' ->
    b >>= \b' ->
    c >>= \c' ->
    return (f a' b' c')

ap :: [a -> b] -> [a] -> [b]
ap f a =
    f >>= \f' ->
    a >>= \a' ->
    return (f' a')
