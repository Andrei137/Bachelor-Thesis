class Functor' f where
  fmap :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap f = foldr (\a acc -> f a : acc) []

instance Functor' Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Functor' ((->) t) where
  fmap f g = \x -> f $ g x

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

instance Functor' (EitherLeft t) where
  fmap _ (EitherLeft (Right b)) = EitherLeft $ Right b
  fmap f (EitherLeft (Left a)) = EitherLeft . Left $ f a

instance Functor' (EitherRight t) where
  fmap _ (EitherRight (Left b)) = EitherRight $ Left b
  fmap f (EitherRight (Right a)) = EitherRight . Right $ f a

class Monad' m where
  (>>~) :: (a -> m b) -> m a -> m b
  return' :: a -> m a

  furry' :: (a -> b) -> m a -> m b
  furry' f = (>>~) (return' . f)

instance Monad' [] where
  (>>~) f = foldr (\a acc -> f a ++ acc) []
  return' a = [a]

instance Monad' Maybe where
  (>>~) _ Nothing = Nothing
  (>>~) f (Just a) = f a
  return' = Just

instance Monad' ((->) t) where
  f >>~ g = \x -> f (g x) x
  return' f = \_ -> f

instance Monad' (EitherLeft t) where
  (>>~) _ (EitherLeft (Right b)) = EitherLeft $ Right b
  (>>~) f (EitherLeft (Left a)) = f a
  return' a = EitherLeft (Left a)

instance Monad' (EitherRight t) where
  (>>~) _ (EitherRight (Left b)) = EitherRight $ Left b
  (>>~) f (EitherRight (Right a)) = f a
  return' a = EitherRight (Right a)

join :: (Monad' m) => m (m a) -> m a
join = (>>~) id

(<**>) :: (Monad' m) => m a -> m (a -> b) -> m b
(<**>) ma = (>>~) (\f -> furry' f ma)

mapM' :: (Monad' m) => [a] -> (a -> m b) -> m [b]
mapM' xs f = foldr (\x acc -> (\y -> furry' (y:) acc) >>~ (f x)) (return' []) xs

sequence' :: (Monad' m) => [m a] -> m [a]
sequence' = flip mapM' id

liftM2' :: (Monad' m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f ma mb = mb <**> furry' (\a -> f a) ma

liftM3' :: (Monad' m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3' f ma mb mc = mc <**> liftM2' f ma mb

liftM4' :: (Monad' m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4' f ma mb mc md = md <**> liftM3' f ma mb mc

newtype State s a = State {
  state :: (s -> (s, a))
}

instance Functor' (State s) where
  fmap f sa = State $ \s ->
    let (s', a) = state sa s
    in (s', f a)

instance Monad' (State s) where
  (>>~) f sa = State $ \s ->
    let (s', a) = state sa s
    in state (f a) s'
  return' a = State $ \s -> (s, a)
