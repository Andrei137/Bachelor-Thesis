### Theory
- The Applicative type class is an extension of the Functor
- It defines how to apply a function with any number of parameters to parameters inside boxes
### Definition
```
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
### Extra
```
<$> :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
or
sequenceA = foldr (liftA2 (:)) (pure [])
```
### Laws
```
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
```
```
sequenceA [[1,2,3],[4,5,6]] =
(:) <$> [1,2,3] <*> sequenceA [[4,5,6]]

sequenceA [[4,5,6]] =
(:) <$> [4,5,6] <*> [[]] =
[[4], [5], [6]]
```
