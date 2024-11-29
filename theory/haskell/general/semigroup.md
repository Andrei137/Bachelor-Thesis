### Theory
- The Semigroup type class is for types that can be accumulated, via an "append" operation
### Definition
```
class Semigroup a where
  (<>) :: a -> a -> a
```
### Example
```
instance Semigroup [a] where
  xs <> ys = xs ++ ys
```
