import Prelude hiding (read)
import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

test' :: (Int, Stack)
test' = runState stackManip [5,8,2]

newtype S s a = S { runS :: s -> (a, s) }
-- runS :: S s a -> s -> (a, s)

instance Functor (S s) where
    fmap f mx = f <$> mx

instance Applicative (S s) where
    pure a = S $ \s -> (a, s)
    mf <*> mx = do
        f <- mf
        f <$> mx

instance Monad (S s) where
    -- return :: a -> S a
    return = pure

    -- (>>=) :: S s a -> (a -> S s b) -> S s b
    S f >>= k =
        -- f :: s -> (a, s)
        -- k :: a -> S s b
        S $ \s -> let (a, s') = f s
                  in runS (k a) s'
        -- k a :: S s b
        -- runS (k, a) :: s -> (b, s)
        -- runS (k, a) s' :: (b, s)

read :: S s s
read = S $ \s -> (s,s)

write :: s -> S s ()
write s = S $ \_ -> ((), s)

foo :: S Int Int
foo = do
    x <- read
    write (x + 1)
    x <- read
    return (x + 3)

test :: (Int, Int)
test = runS foo 0

{-

instance Monad (\s -> (_, s)) where
    -- return :: a -> (s -> (a,s))
    return a = \s -> (a,s)

    -- (>>=) :: (s -> (a,s)) -> (a -> (s -> (b, s))) -> (s -> (b,s))
    f >>= k = \s -> let (a, s') = f s in k a s'

read s = (s,s)
write s = \_ -> ((), s)

foo =
    read >>= \x ->
    write (x+1) >>= \_ ->
    read >>= \x ->
    return (x+3)

-- equivalently
foo = read >>= (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3))))

test = foo 0
     = (read >>= (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3))))) 0
     = (\s -> let (a, s') = read s
              in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3)))) a s') 0
     = let (a, s') = read 0
       in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3)))) a s'
     = let (a, s') = (0, 0)
       in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3)))) a s'
     = (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return (x+3)))) 0 0
     = write (0+1) >>= (\_ -> read >>= (\x -> return (x+3))) 0
     = (\s -> let (a, s') = write 1
              in (\_ -> read >>= (\x -> return (x+3))) a s') 0
     = let (a, s') = ((), 1)
       in (\_ -> read >>= (\x -> return (x+3))) a s'
     = (\_ -> read >>= (\x -> return (x+3))) () 1
     = read >>= (\x -> return (x+3)) 1
     = (\s -> let (a, s') = read s
              in (\x -> return (x+3)) a s') 1
     = let (a, s') = read 1
       in (\x -> return (x+3)) a s'
     = let (a, s') = (1,1)
       in (\x -> return (x+3)) a s'
     = (\x -> return (x+3)) 1 1
     = return (1+3) 1
     = return 4 1
     = (\s -> (4, s)) 1
     = (4, 1)
-}
