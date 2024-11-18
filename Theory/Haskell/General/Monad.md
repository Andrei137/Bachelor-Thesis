### Theory
- The Monad type class is an extension of Applicatives
- It defines how to take a value inside a box and apply it to a function that takes a normal value and returns another value inside that box
### Definition
```
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```
### Extra
```
(>>) :: m a -> m b -> m b
x >> y = x >>= \_ -> y

fail :: String -> m a
fail msg = error msg

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

-- fmap
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))
liftM f m = do
    x <- m
    return (f x)

-- <*>
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m
m >>= f is the same as join (fmap f m)

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```
### do Notation
```
foo' :: Maybe String
foo' = Just 3 >>= (\x ->
       Just "!" >>= (\y ->
       Just (show x ++ y)))

foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
```
### Laws
```
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

f <=< return = f
return <=< f = f
f <=< (g <=< h) = (f <=< g) <=< h
```
### Writer
- Definition
```
newtype Writer w a = Writer { runWriter :: (a, w) }
```
- Implementation
```
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```
### State
- Definition
```
newtype State s a = State { runState :: s -> (a, s) }
```
- Implementation
```
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState

get = state $ (\s) -> (s,s)
put newState = state $ \s -> ((), newState)
```
- Explanation
```
h is a function that takes a state s and returns a new state (a,s)
for push 3, h would be \s -> ((), 3:s)

f is a function that takes a, the result of a computation, and returns a State s b
for example, f is \_ -> pop

g is the function inside the State returned by f a
for the above f, g is pop
```
- Example
```
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

test :: Stack
test = runState stackManip [5,8,2]
```
- Explanation
```
instance Monad (\s -> (_, s)) where
    -- return :: a -> (s -> (a, s))
    return a = \s -> (a, s)

    -- (>>=) :: (s -> (a,s)) -> (a -> (s -> (b, s))) -> (s -> (b,s))
    f >>= k = \s -> let (a, s') = f s in k a s'

pop (x:xs) = (x,xs)
push a xs = ((), a:xs)
stackManip = push 3 >>= (\_ -> pop >>= (\_ -> pop))

test = stackManip [5,8,2]
     = push 3 >>= (\_ -> pop >>= (\_ -> pop))
     = (\s -> let (a, s') = push 3 s
              in (\_ -> pop >>= (\_ -> pop) a s') [5,8,2]
     = let (a, s') = push 3 [5,8,2]
       in (\_ -> pop >>= (\_ -> pop) a s')
     = let (a, s') = push ((), [3,5,8,2])
       in (\_ -> pop >>= (\_ -> pop) a s')
     = (\_ -> pop >>= (\_ -> pop) () [3,5,8,2]
     = pop >>= (\_ -> pop) [3,5,8,2]
     = (\s -> let (a, s') = pop s
              in (\_ -> pop) a s') [3,5,8,2]
     = let (a, s') = pop [3,5,8,2]
       in (\_ -> pop) a s'
     = let (a, s') = (3, [5,8,2])
       in (\_ -> pop) a s'
     = (\_ -> pop) 3 [5,8,2]
     = pop [5,8,2]
     = (5, [8,2])
```
