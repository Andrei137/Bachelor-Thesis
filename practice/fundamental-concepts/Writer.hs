addTwo'' :: Int -> ([String], Int)
addTwo'' x = (["adding 2..."], x + 2)

augmentAndStringify'' :: Int -> Int -> ([String], String)
augmentAndStringify'' x y =
  let (xLog, x') = addTwo'' x
      (yLog, y') = addTwo'' y
  in (["augmenting..."] ++ xLog ++ yLog ++ ["stringifying..."], show (x' + y'))

{-
    :: Int -> ([String], Int)
    :: Int -> Int -> ([String], String)
    :: ([String], a)
-}

newtype Logger a = Logger { runLogger :: ([String], a) }

instance Functor Logger where
    fmap f la = Logger $
        let (msg, a) = runLogger la
        in (msg, f a)

instance Applicative Logger where
    pure a = Logger ([], a)
    lf <*> la = Logger $
        let (msg1, f) = runLogger lf
            (msg2, a) = runLogger la
        in (msg1 ++ msg2, f a)

instance Monad Logger where
    return = pure
    la >>= f = Logger $
        let (msg1, a) = runLogger la
            (msg2, b) = runLogger $ f a
        in (msg1 ++ msg2, b)

-- Logs one message
log'' :: String -> Logger ()
log'' msg = Logger ([msg], ())

-- Logs several messages
logs' :: [String] -> Logger ()
logs' msgs = Logger (msgs, ())

addTwo' :: Int -> Logger Int
addTwo' x = do
    log'' "adding 2..."
    return $ x + 2

augmentAndStringify' :: Int -> Int -> Logger String
augmentAndStringify' x y = do
    log'' "augmenting..."
    x' <- addTwo' x
    y' <- addTwo' y
    log'' "stringifying..."
    return $ show (x' + y')

newtype Writer log a = Writer { runWriter :: (log, a) }

instance Functor (Writer log) where
    fmap f wa = Writer $
        let (log, a) = runWriter wa
        in (log, f a)

instance Monoid log => Applicative (Writer log) where
    pure a = Writer $ (mempty, a)
    wf <*> wa = Writer $
        let (log1, f) = runWriter wf
            (log2, a) = runWriter wa
        in (log1 `mappend` log2, f a)

instance Monoid log => Monad (Writer log) where
    return = pure
    wa >>= f = Writer $
        let (log1, a) = runWriter wa
            (log2, b) = runWriter $ f a
        in (log1 `mappend` log2, b)

-- Logs one message
tell :: log -> Writer log ()
tell log = Writer (log, ())

log' :: String -> Writer [String] ()
log' log = tell [log]

addTwo :: Int -> Writer [String] Int
addTwo x = do
    log' "adding 2..."
    return $ x + 2

augmentAndStringify :: Int -> Int -> Writer [String] String
augmentAndStringify x y = do
    log' "augmenting..."
    x' <- addTwo x
    y' <- addTwo y
    log' "stringifying..."
    return $ show (x' + y')

-- Run a function on the log
censor :: (log -> log) -> Writer log a -> Writer log a
censor f wa = Writer $
    let (log, a) = runWriter wa
    in (f log, a)

listen :: Writer log a -> Writer log (a, log)
listen wa = Writer $
    let (log, a) = runWriter wa
    in (log, (a, log))
