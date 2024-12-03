import Control.Monad

data TurnstileState = Locked | Unlocked
    deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
    deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
        (a3, s3) = push s2
        (a4, s4) = coin s3
        (a5, s5) = push s4
    in ([a1, a2, a3, a4, a5], s5)

regularPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
    in ([a1, a2], s2)

distractedPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
distractedPerson = (\(a, s) -> ([a], s)) . coin

hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
hastyPerson s0 =
    let (a1, s1) = push s0
    in case s0 of
        Unlocked -> ([a1], s1)
        Locked -> do
            let (a2, s2) = coin s1
                (a3, s3) = push s2
            ([a1, a2, a3], s3)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let (a1, s1) = regularPerson s0
        (a2, s2) = hastyPerson s1
        (a3, s3) = distractedPerson s2
        (a4, s4) = hastyPerson s3
    in (concat [a1, a2, a3, a4], s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair first s0
    | first == True = verify regularPerson
    | otherwise = verify distractedPerson
    where
    verify person =
        let (_, s1) = person s0
            (a2, s2) = push s1
        in (s1 == Unlocked, s2)

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure x = state $ \s -> (x, s)
    (<*>) = ap

instance Monad (State s) where
    return = pure
    ma >>= f = state $ \s ->
        let (a, s') = runState ma s
        in runState (f a) s'

coinS :: State TurnstileState TurnstileOutput
coinS = do
    s <- get
    put Unlocked
    return Thank

pushS :: State TurnstileState TurnstileOutput
pushS = do
    s <- get
    put Locked
    case s of
        Locked -> return Tut
        Unlocked -> return Open

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

regularPersonS' :: State TurnstileState [TurnstileOutput]
regularPersonS' = sequence [coinS, pushS]

distractedPersonS' :: State TurnstileState [TurnstileOutput]
distractedPersonS' = sequence [coinS]

hastyPersonS' :: State TurnstileState [TurnstileOutput]
hastyPersonS' = do
    a1 <- pushS
    case a1 of
        Open -> return [Open]
        Tut -> do
            rest <- sequence [coinS, pushS]
            return $ a1:rest

luckyPairS :: Bool -> State TurnstileState Bool
luckyPairS first
    | first == True = verify regularPersonS'
    | otherwise = verify distractedPersonS'
    where
    verify person = do
        person
        a2 <- pushS
        return (a2 == Open)

evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)

get :: State s s
get = state $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f = do
    s <- get
    return (f s)

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)

testTurnstile :: State TurnstileState Bool
testTurnstile = do
    s <- get
    put Locked
    check1 <- pushS
    put Unlocked
    check2 <- pushS
    put Locked
    coinS
    check3 <- get
    put s
    return (check1 == Tut && check2 == Open && check3 == Unlocked)

data TurnstileInput = Coin | Push
    deriving (Eq, Show)

turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = state . turn where
    turn Coin _        = (Thank, Unlocked)
    turn Push Unlocked = (Open,  Locked)
    turn Push Locked   = (Tut,   Locked)

getsThroughS :: TurnstileInput -> State TurnstileState Bool
getsThroughS input = do
    output <- turnS input
    return $ output == Open

countOpens :: [TurnstileInput] -> State TurnstileState Int
countOpens = foldM incIfOpens 0
    where
    incIfOpens :: Int -> TurnstileInput -> State TurnstileState Int
    incIfOpens n i = do
        g <- getsThroughS i
        if g then return (n+1) else return n

{-
    evalState (replicateM 6 pushS) Unlocked
    evalState (mapM turnS [Coin, Push, Push, Coin, Push]) Locked
    evalState (filterM getsThroughS [Push, Coin, Coin, Push, Push, Coin, Push]) Locked
    evalState (countOpens [Coin, Push, Coin, Push, Push, Coin, Push]) Locked
-}

regularPersonS :: State TurnstileState [TurnstileOutput]
regularPersonS = mapM turnS [Coin, Push]

distractedPersonS :: State TurnstileState [TurnstileOutput]
distractedPersonS = mapM turnS [Coin]

hastyPersonS :: State TurnstileState [TurnstileOutput]
hastyPersonS = do
    a1 <- turnS Push
    case a1 of
        Open -> return [Open]
        Tut -> do
            rest <- mapM turnS [Coin, Push]
            return $ a1:rest

tuesdayS :: State TurnstileState [TurnstileOutput]
tuesdayS = fmap concat $ sequence [regularPersonS, hastyPersonS, distractedPersonS, hastyPersonS]
