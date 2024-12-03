-- Reverse a list, and increase a count of function calls
reverseWithCount' :: Int -> [a] -> (Int, [a])
reverseWithCount' funcCount list = (funcCount + 1, reverse list)

appendReversedWithCount' :: Int -> [a] -> [a] -> (Int, [a])
appendReversedWithCount' funcCount list1 list2 =
    let (funcCount', revList1) = reverseWithCount' funcCount list1
        (funcCount'', revList2) = reverseWithCount' funcCount' list2
    in (funcCount'' + 1, revList1 ++ revList2)

append3ReversedWithCount' :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount' funcCount list1 list2 list3 =
    let (funcCount', revList1) = reverseWithCount' funcCount list1
        (funcCount'', revList2) = reverseWithCount' funcCount' list2
        (funcCount''', revList3) = reverseWithCount' funcCount'' list3
    in (funcCount''' + 1, revList1 ++ revList2 ++ revList3)

{-
    :: Int -> [a] -> (Int, [a])
    :: Int -> [a] -> [a] -> (Int, [a])
    :: Int -> [a] -> [a] -> [a] -> (Int, [a])
    :: state -> (state, a)
-}

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
    fmap f sa = State $ \s ->
        let (s', a) = runState sa s
        in (s', f a)

instance Applicative (State s) where
    pure a = State $ \s -> (s, a)
    sf <*> sa = State $ \s ->
        let (s', f) = runState sf s
            (s'', a) = runState sa s'
        in (s'', f a)

instance Monad (State s) where
    return = pure
    sa >>= f = State $ \s ->
        let (s', a) = runState sa s
        in runState (f a) s'

-- Retrieve the current state
get :: State s s
get = State $ \s -> (s, s)

-- Replace the current state with the given value
put :: s -> State s ()
put s = State $ \_ -> (s, ())

-- Update the current state using the given function
modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put $ f s

reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = do
    modify (+1)
    return $ reverse list

appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount list1 list2 = do
    revList1 <- reverseWithCount list1
    revList2 <- reverseWithCount list2
    modify (+1)
    return $ revList1 ++ revList2

append3ReversedWithCount :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithCount list1 list2 list3 = do
    revList1 <- reverseWithCount list1
    revList2 <- reverseWithCount list2
    revList3 <- reverseWithCount list3
    modify (+1)
    return $ revList1 ++ revList2 ++ revList3
