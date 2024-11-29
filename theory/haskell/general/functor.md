### Theory
- The Functor type class is for things that can be mapped over
- It defines how to apply an unary function to a value inside a box 
### Definition
```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
### Laws
```
fmap id = id
fmap (f . g) = fmap f . fmap g
```
