### Functor
- Implementation
```
instance Functor ((->) r) where
  fmap f g = \x -> f (g x)
  fmap = (.)
```
- Examples
```
ghci> fmap (*3) (+100) 1
303
ghci> fmap (`mod` 2) (`div` 5) 15
1
```
- r -> a can be rewritten as (->) r a, thus the box is (->) r
- So fmap has type (a -> b) -> ((->) r a) -> ((->) r b), which is actually (a -> b) -> (r -> a) -> (r -> b)
- We can see that it matches the composition, but we can think of it in a better way
- fmap esetially takes something out of the box, applies the function to it and returns it inside the box
- To take a function g out of its box, we need to rewrite g as \x -> g x, and we know the box is (->) r, so that something becomes g x
- Then we apply the function f to it, and we get f (g x)
- Finally, we return it inside the box, so we get \x -> f (g x), which is the composition
### Applicative
- Implementation
```
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
```
- Examples
```
ghci> (+) <$> (+3) <*> (*100) $ 5
508
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```
- Explanation
```
Let f = (+) <$> (+3) <*> (*100)

f = (+) <$> (+3) <*> (*100)
  = (((+) <$> (+3)) <*> (*100))        # add paranthesis
  = A <*> (*100)                       # replace the innermost one with A
  = (\x -> ((x + 3) +)) <*> (*100)     # replace A with the result
  = pure ((x + 3) +) <*> (*100)        # pure f = (\_ -> f)
  = ((x + 3) +) <$> (*100)             # pure f <*> something = f <$> something
  = fmap ((x + 3) +) (*100)            # f <$> something = fmap f something
  = fmap ((x + 3) +) (\x -> x * 100)   # writing (*100) in the "box" form
  = \x -> (x + 3) + (x * 100)          # applying fmap (see the Functor section)

A = (+) <$> (+3)
  = fmap (+) (+3)          # f <$> something = fmap f something
  = fmap (+) (\x -> x + 3) # writing (+3) in the "box" form
  = \x -> (+) (x + 3)      # applying fmap
  = \x -> ((x + 3) +)      # writing it as a section

So f 5 = (5 + 3) + (5 * 100) = 508
```
- The minimal context a value can be in is (\\_ -> value), a function that returns it no matter the argument
- Applying a function to other functions means each one will be applied to a parameter of the first function, from left to right
- For example, (+) <$> (\*\*2) <\*> (/ 12) means when we give it an x, we will get (x\*\*2) + (x / 12)
### Monad
- Implementation
```
instance Monad ((->) r) where
  return x = \_ -> x
  h <<= f = \w -> f (h w) w
```
- Examples
```
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
```
