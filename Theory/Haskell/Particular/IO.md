### Functor
- Implementation
```
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```
- Examples
```
ghci> fmap (*10) (return 5)
50
ghci> fmap (map toUpper) (return "hello")
"HELLO"
```
- Mapping over (IO something) takes that something out of the box, applies the function to it and puts the result back inside the box
### Applicative
- Implementation
```
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```
- Examples
```
ghci> pure 5 :: [Int]
[5]
ghci> (return (,)) <*> pure "Result" <*> pure 42
("Result",42)
ghci> (**) <$> pure 2 <*> (return 3)
8.0
```
- The minimal context a value can be in is (IO value), which is given by return
- Applying (IO f) to (IO value) means taking the function and the value out of the box and putting back into the box the result of f applied to the value
