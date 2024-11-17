### Important
- Either cannot be a functor, because Functors require only one type parameter, and Either takes two
- However, (Either a) can be a functor
### Functor
- Implementation
```
instance Functor (Either a) where
    fmap f (Left x) = Left x
    fmap f (Right x) = Right (f x)
```
- Examples
```
ghci> fmap (+3) (Left "Test")
Left ("Test")
ghci> fmap (+3) (Right 10)
Just 13
```
- Mapping over an empty box (Left something) gives an empty box
- Mapping over (Right something) takes that something out of the box, applies the function to it and puts the result back inside the box
### Applicative
- Implementation
```
instance Applicative (Either a) where
    pure = Right
    (Left something) <*> _ = Left something
    (Right f) <*> something = fmap f something
```
- Examples
```
ghci> pure 5 :: Either String
Right 5
ghci> (Left "Test") <*> (Right 3)
Left "Test"
ghci> (Right (+5)) <*> (Right 5)
Right 10
ghci> (*) <$> (Right 3) <*> (Right 22)
Just 66
```
- The minimal context a value can be in is (Right value), (Left something) means the lack of a value
- If there is no function inside the box (Left something), the result is an empty box
- Applying (Right f) to (Right value) means taking the function out of the box and using fmap with it and (Right value)
### Monad
- Implementation
```
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```
