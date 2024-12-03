{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Trans
import Control.Monad.State.Class

validateData1' :: Int -> IO (Maybe Int)
validateData1' x = if x > 0 then pure $ Just x else pure Nothing

validateData2' :: String -> IO (Maybe String)
validateData2' x = if length x > 0 then pure $ Just x else pure Nothing

validateForm' :: Int -> String -> IO (Maybe (Int, String))
validateForm' rawData1 rawData2 = do
    data1m <- validateData1' rawData1
    case data1m of
        Nothing -> pure Nothing
        Just data1 -> do
            data2m <- validateData2' rawData2
            case data2m of
                Nothing -> pure Nothing
                Just data2 -> pure $ Just (data1, data2)

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
    fmap f mia = MaybeIO $ do
        ma <- runMaybeIO mia
        return $ f <$> ma

instance Applicative MaybeIO where
    pure a = MaybeIO . return $ Just a
    mif <*> mia = MaybeIO $ do
        mf <- runMaybeIO mif
        ma <- runMaybeIO mia
        return $ mf <*> ma

instance Monad MaybeIO where
    return = pure
    mia >>= f = MaybeIO $ do
        ma <- runMaybeIO mia
        case ma of
            Nothing -> return Nothing
            Just a -> runMaybeIO $ f a

validateData1 :: Int -> MaybeIO Int
validateData1 x = MaybeIO $ if x > 0 then pure $ Just x else pure Nothing

validateData2 :: String -> MaybeIO String
validateData2 x = MaybeIO $ if length x > 0 then pure $ Just x else pure Nothing

validateForm :: Int -> String -> MaybeIO (Int, String)
validateForm rawData1 rawData2 = do
    data1 <- validateData1 rawData1
    data2 <- validateData2 rawData2
    return (data1, data2)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) (MaybeT mma) f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a -> runMaybeT (f a)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
    fmap f (StateT ma) = StateT $ \s -> do
        (a, s') <- ma s
        return (f a, s')

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    (StateT mf) <*> (StateT ma) = StateT $ \s -> do
        (f, s') <- mf s
        (a, s'') <- ma s'
        return (f a, s'')

instance Monad m => Monad (StateT s m) where
    return = pure
    (StateT ma) >>= f = StateT $ \s -> do
        (a, s') <- ma s
        runStateT (f a) s'

newtype ReaderT cfg m a = ReaderT { runReaderT :: cfg -> m a }

instance Monad m => Functor (ReaderT cfg m) where
    fmap f (ReaderT ma) = ReaderT $ \cfg -> fmap f (ma cfg)

instance Monad m => Applicative (ReaderT cfg m) where
    pure a = ReaderT $ \_ -> return a
    (ReaderT mf) <*> (ReaderT ma) = ReaderT $ \cfg -> do
        f <- mf cfg
        a <- ma cfg
        return $ f a

instance Monad m => Monad (ReaderT cfg m) where
    return = pure
    (ReaderT ma) >>= f = ReaderT $ \cfg -> do
        a <- ma cfg
        runReaderT (f a) cfg

type AMonadStack a = StateT Int (ReaderT String (MaybeT IO)) a

-- for MaybeT
nothing :: Applicative m => MaybeT m a
nothing = MaybeT $ pure Nothing

-- for StateT
get' :: Applicative m => StateT s m s
get' = StateT $ \s -> pure (s, s)

put' :: Applicative m => s -> StateT s m ()
put' s = StateT $ \_ -> pure ((), s)

-- for ReaderT
ask :: Applicative m => ReaderT cfg m cfg
ask = ReaderT pure

local :: (cfg -> cfg') -> ReaderT cfg' m a -> ReaderT cfg m a
local f (ReaderT ma) = ReaderT $ ma . f

{-
    class MonadTrans trans where
        lift :: Monad m => m a -> trans m a
-}

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> ma >>= \a -> return (a, s)

instance MonadTrans (ReaderT cfg) where
    lift ma = ReaderT $ const ma

validateInput :: MaybeT IO String
validateInput = do
    line <- lift getLine
    if length line > 0 then return line else nothing

foo' :: StateT Int (ReaderT String (MaybeT IO)) ()
foo' = do
    -- need to do some IO in this stack
    input <- lift $ lift $ lift getLine
    if length input > 0 then return () else lift $ lift $ lift $ putStrLn "Empty input"

{-
    class Monad m => MonadState s m where
        get :: m s
        put :: s -> m ()
-}

-- StateT can implement MonadState directly...
instance Monad m => MonadState s (StateT s m) where
    get = StateT $ \s -> pure (s, s)
    put s = StateT $ \_ -> pure ((), s)

-- ...and if the inner monad supports statefulness, other monad transformers can simply delegate the state operations downward
instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put s = lift $ put s

instance MonadState s m => MonadState s (ReaderT cfg m) where
    get = lift get
    put s = lift $ put s

foo :: StateT Int IO ()
foo = do
  state <- get
  put (state + 1)

-- ...as well as this one
bar :: MaybeT (StateT Int IO) ()
bar = do
  state <- get
  put (state + 1)

-- ...or even fully polymorphic
baz :: MonadState Int m => m ()
baz = do
  state <- get
  put (state + 1)
