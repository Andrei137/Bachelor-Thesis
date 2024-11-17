### Functor
- Implementation
```
Instance Functor Ziplist where
    fmap f (ZipList xs) = ZipList (fmap f xs)
```
- Examples
```
ghci> fmap (*10) (ZipList [])
ZipList []
ghci> fmap (+3) (ZipList [1,2,3])
ZipList [4,5,6]
```
- Mapping over an empty box (Ziplist []) gives an empty box
- Mapping over Ziplist [something] takes that something out of the box, applies the function to each value and puts the results back inside the box
### Applicative
- Implementation
```
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
```
- Examples
```
ghci> take 4 . getZipList $ pure 5
[5,5,5,5]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
```
- The minimal context a value can be in is an infinite list of itself, to be able to produce a result on every finite position
- Applying ZipList fs to ZipList xs means taking each function and value out of the boxes and putting the results of each pair inside the box
