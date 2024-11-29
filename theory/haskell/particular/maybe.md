### Functor
- Implementation
```
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)
```
- Examples
```
ghci> fmap (+3) Nothing
Nothing
ghci> fmap (+3) (Just 2)
Just 5
```
- Mapping over an empty box (Nothing) gives an empty box
- Mapping over (Just something) takes that something out of the box, applies the function to it and puts the result back inside the box
### Applicative
- Implementation
```
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```
- Examples
```
ghci> pure 5 :: Maybe Int
Just 5
ghci> Nothing <*> (Just 3)
Nothing
ghci> (Just (+3)) <*> (Just 5)
Just 8
ghci> (Just (+)) <*> (Just 10) <*> (Just 3)
Just 13
ghci> (*) <$> (Just 3) <*> (Just 11)
Just 33

```
- The minimal context a value can be in is (Just value), Nothing means the lack of a value
- If there is no function inside the box (Nothing), the result is an empty box
- Applying (Just f) to (Just value) means taking the function out of the box and using fmap with it and (Just value)
### Monoid
- Implementation
```
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```
- Examples
```
ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
```
- If the contents of the Maybe is not an instance of Monoid, this will not work.
- However, we can get define a new type that discards the second parameter, and another one that discards the first
```
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

newtype Last a = Last { getLast :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

instance Monoid (Last a) where
    mempty = Last Nothing
    Last Nothing `mappend` x = x
    x `mappend` Last Nothing = x
    Last (Just a) `mappend` Last (Just b) = Last (Just b)
```
### Monad
- Implementation
```
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
```
- Examples
```
ghci> return "WHAT" :: Maybe String
Just "WHAT"
ghci> Just 9 >>= \x -> return (x*10)
Just 90
ghci> Nothing >>= \x -> return (x*10)
Nothing
```
