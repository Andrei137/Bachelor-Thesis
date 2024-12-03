{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class

isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

getPassphrase' :: IO (Maybe String)
getPassphrase' = do
    s <- getLine
    if isValid s then return $ Just s
                 else return Nothing

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance Monad m => Applicative (MaybeT m) where
    pure  = MaybeT . return . Just
    (<*>) = ap

instance Monad m => Monad (MaybeT m) where
    return = pure

    mx >>= f = MaybeT $ do
        x <- runMaybeT mx
        case x of
            Nothing    -> return Nothing
            Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    mx <|> my = MaybeT $ do
        x <- runMaybeT mx
        case x of
            Nothing    -> runMaybeT my
            Just _     -> return x

instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)

getPassphrase :: MaybeT IO String
getPassphrase = do
    s <- lift getLine
    guard (isValid s)
    return s

askPassphrase :: MaybeT IO ()
askPassphrase = do
    lift $ putStrLn "Insert your new passphrase:"
    value <- msum $ repeat getPassphrase
    lift $ putStrLn "Storing in database..."

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    (<*>) = ap

instance (Monad m) => Monad (StateT s m) where
    return = pure
    (StateT x) >>= f = StateT $ \s -> do
        (v, s') <- x s
        runStateT (f v) s'

instance (Monad m) => MonadState s (StateT s m) where
  get   = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)

instance (Monad m, Alternative m) => Alternative (StateT s m) where
    empty = StateT $ \_ -> empty
    (StateT x1) <|> (StateT x2) = StateT $ \s -> x1 s <|> x2 s

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans (StateT s) where
    lift c = StateT $ \s -> c >>= (\x -> return (x, s))

state :: MonadState s m => (s -> (a, s)) -> m a
state f = do
    s <- get
    let (a, s') = f s
    put s'
    return a
