### Theory
- The MonadPlus type class is for Monads that can also act as monoids
### Definition
```
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
```
### Extra
```
guard :: (MonadPlus m) => Bool -> m ()
    guard True = return ()
    guard False = mzero
```
