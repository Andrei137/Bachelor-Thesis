### Theory
- The Monoid type class is for types whose values can be combined together with a binary operation
- It is an extension of the Semigroup type, including an identity element that doesn't change other values when used with the binary function
### Definition
```
class Monoid m where
    mempty :: m
```
### Extra
```
mappend :: m -> m -> m
mappend = <>

mconcat :: [m] -> m
mconcat = foldr mappend mempty
```
### Laws
```
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```
### Product and Sum
- Definition
```
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

newtype Sum a = Product { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
```
- Implementation
```
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
```
- Examples
```
ghci> getProduct $ Product 3 `mappend` Product 9
27
ghci> getProduct . mconcat . map Product $ [3,4,2]
24
ghci> getSum $ mempty `mappend` Sum 3
3
ghci> getSum . mconcat . map Sum $ [1,2,3]
6
```
### Any and All
- Definition
```
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
```
- Implementation
```
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)
```
- Examples
```
ghci> getAny $ Any True `mappend` Any False
True
ghci> getAny . mconcat . map Any $ [False, False, False, True]
True
ghci> getAll $ mempty `mappend` All True
True
ghci> getAll . mconcat . map All $ [True, True, False]
False
```
### Ordering
- Implementation
```
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
```
- Examples
```
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
```
### Foldable
- Example
```
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x  `mappend`
                             F.foldMap f r
```
- Usage
```
testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True
ghci> F.foldMap (\x -> [x]) testTree
[1,3,6,5,8,9,10]
```
