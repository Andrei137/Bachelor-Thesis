### Functor
- Implementation
```
instance Functor [] where
    fmap = map
```
- Examples
```
ghci> fmap (*10) []
[]
ghci> fmap (+3) [1,2,3]
[4,5,6]
```
- Mapping over an empty box ([]) gives an empty box
- Mapping over [something] takes that something out of the box, applies the function to each value and puts the results back inside the box
### Applicative
- Implementation
```
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```
- Examples
```
ghci> pure 5 :: [Int]
[5]
ghci> [] <*> [1,2,3]
[]
ghci> [(+3), (*2)] <*> [3,2]
[6,5,6,4]
ghci> [(*), (+)] <*> [1,2] <*> [3,4]
[3,4,6,8,4,5,5,6]
ghci> (**) <$> [3,4] <*> [1,2]
[3.0,9.0,4.0,16.0]
```
- The minimal context a value can be in is [value], [] means the lack of a value
- Applying [fs] to [xs] means taking each function and value out of the boxes, applying each function to each value and putting the results inside the box
### Monoid
- Implementation
```
mempty = []
mappend = (++)
```
- Examples
```
ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]
ghci> ("one" `mappend` "two") `mappend` "tree"
"onetwotree"
ghci> mconcat [[1,2],[3,6],[9]]
[1,2,3,6,9]
```
### Monad
- Implementation
```
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
```
- Examples
```
ghci> [3,4,5] >>= \x -> [x,-x]
[3,-3,4,-4,5,-5]
ghci> [] >>= \x -> ["bad","mad","rad"]
ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
[]
```
