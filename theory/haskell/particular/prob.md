### Definition
```
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show
```
### Implementation
```
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure x = Prob [(x,1%1)]
    mf <*> ma = do
        f <- mf
        f <$> ma

instance Monad Prob where
    return = pure
    m >>= f = do
        flatten (fmap f m)
        where
            flatten (Prob xs) = Prob $ concat $ map multAll xs
            multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs
```
### Example
```
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a, b, c])
```
### Explanation
```
flipThree = coin >>= (\a -> coin >>= (\b -> loadedCoin >>= (\c -> return (all (==Tails) [a,b,c]))))

Let A = (all (==Tails) [a,b,c])
    B sideA = flatten (fmap (\b -> loadedCoin >>= (\c -> return A[a:=sideA])) coin)
    C sideA sideB = flatten (fmap (\c -> return A[a:=sideA, b:=sideB]) loadedCoin

flipThree
    = flatten (fmap (\a -> coin >>= (\b -> loadedCoin >>= (\c -> return A))) coin)
    = flatten (fmap (\a -> coin >>= (\b -> loadedCoin >>= (\c -> return A))) Prob [(Heads, 1%2), (Tails, 1%2)])
    = flatten (Prob $ map (\(x, p) -> ((\a -> coin >>= (\b -> loadedCoin >>= (\c -> return A)) x, p) [(Heads, 1%2), (Tails, 1%2)])
    = flatten (Prob $ [(coin >>= (\b -> loadedCoin >>= (\c -> return A[a:=Heads])), 1%2),
                       (coin >>= (\b -> loadedCoin >>= (\c -> return A[a:=Tails])), 1%2])
    = flatten (Prob $ [(flatten (fmap (\b -> loadedCoin >>= (\c -> return A[a:=Heads])) coin, 1%2)),
                       (flatten (fmap (\b -> loadedCoin >>= (\c -> return A[a:=Tails])) coin, 1%2)])
    = flatten (Prob $ [(B Heads, 1%2), (B Tails, 1%2)])
    = ...
    = Prob [(A[a:=Heads, b:=Heads, c:=Heads], 1%40),
            (A[a:=Heads, b:=Heads, c:=Tails], 9%40),
            (A[a:=Heads, b:=Tails, c:=Heads], 1%40),
            (A[a:=Heads, b:=Tails, c:=Tails], 9%40),
            (A[a:=Tails, b:=Heads, c:=Heads], 1%40),
            (A[a:=Tails, b:=Heads, c:=Tails], 9%40),
            (A[a:=Tails, b:=Tails, c:=Heads], 1%40),
            (A[a:=Tails, b:=Tails, c:=Tails], 9%40)]
    = Prob [(False, 1%40),
            (False, 9%40),
            (False, 1%40),
            (False, 9%40),
            (False, 1%40),
            (False, 9%40),
            (False, 1%40),
            (True, 9%40)]

B sideA
    = flatten (fmap (\b -> loadedCoin >>= (\c -> return A[a:=sideA])) coin)
    = flatten (Prob $ map (\(x, p) -> (\b -> loadedCoin >>= (\c -> return A[a:=sideA]) x, p) [(Heads, 1%2), (Tails, 1%2)])
    = flatten (Prob $ [(loadedCoin >>= (\c -> return A[a:=sideA, b:=Heads]), 1%2),
                       (loadedCoin >>= (\c -> return A[a:=sideA, b:=Tails]), 1%2)])
    = flatten (Prob $ [(flatten (fmap (\c -> return A[a:=sideA, b:=Heads]) loadedCoin, 1%2)
                       (flatten (fmap (\c -> return A[a:=sideA, b:=Tails]) loadedCoin, 1%2)])
    = flatten (Prob $ [(C sideA Heads, 1%2), (C sideA Tails, 1%2)])
    = flatten (Prob $ [(Prob [(A[a:=sideA, b:=Heads, c:=Heads], 1%10),
                             (A[a:=sideA, b:=Heads, c:=Tails], 9%10])], 1%2),
                       (Prob [(A[a:=sideA, b:=Tails, c:=Heads], 1%10),
                             (A[a:=sideA, b:=Tails, c:=Tails], 9%10)]], 1%2)
    = Prob $ concat $ map multAll [(Prob [(A[a:=sideA, b:=Heads, c:=Heads], 1%10),
                                         (A[a:=sideA, b:=Heads, c:=Tails], 9%10])], 1%2),
                                   (Prob [(A[a:=sideA, b:=Tails, c:=Heads], 1%10),
                                         (A[a:=sideA, b:=Tails, c:=Tails], 9%10)]], 1%2)
    = Prob $ concat [[(A[a:=sideA, b:=Heads, c:=Heads], 1%20),
                      (A[a:=sideA, b:=Heads, c:=Tails], 9%20)],
                     [(A[a:=sideA, b:=Tails, c:=Heads], 9%20),
                      (A[a:=sideA, b:=Tails, c:=Tails], 9%20)]]
    = Prob [(A[a:=sideA, b:=Heads, c:=Heads], 1%20),
            (A[a:=sideA, b:=Heads, c:=Tails], 9%20),
            (A[a:=sideA, b:=Tails, c:=Heads], 1%20),
            (A[a:=sideA, b:=Tails, c:=Tails], 9%20)]

C sideA sideB
    = flatten (fmap (\c -> return A[a:=sideA, b:=sideB]) loadedCoin
    = flatten (Prob $ map (\x, p) -> (\c -> return A[a:=sideA, b:=sideB]) x, p) [(Heads, 1%10), (Tails, 9%10)])
    = flatten (Prob $ [(return A[a:=sideA, b:=sideB, c:=Heads], 1%10),
                       (return A[a:=sideA, b:=sideB, c:=Tails], 9%10)])
    = flatten (Prob $ [(Prob (A[a:=sideA, b:=sideB, c:=Heads], 1%1), 1%10),
                       (Prob (A[a:=sideA, b:=sideB, c:=Tails], 1%1), 9%10])
    = Prob $ concat $ map multAll [(Prob (A[a:=sideA, b:=sideB, c:=Heads], 1%1), 1%10),
                                   (Prob (A[a:=sideA, b:=sideB, c:=Tails], 1%1), 9%10]
    = Prob $ concat [[(A[a:=sideA, b:=sideB, c:=Heads], 1%10)], [(A[a:=sideA, b:=sideB, c:=Tails], 9%10)]]
    = Prob [(A:=sideA, b:=sideB, c:=Heads], 1%10),
            (A:=sideA, b:=sideB, c:=Tails], 9%10)]
```
