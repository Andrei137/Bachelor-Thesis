import Data.Ratio
import Control.Monad
import Data.List (all)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

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

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

joinProbs :: [(Bool, Rational)] -> [(Bool, Rational)]
joinProbs [] = [(True, 0%1), (False, 0%1)]
joinProbs ((value, p):xs) =
    case value of
        True -> [(true, pt + p), (false, pf)]
        False -> [(true, pt), (false, pf + p)]
    where ((true, pt):(false, pf):[]) = joinProbs xs

flipThree :: [(Bool, Rational)]
flipThree = joinProbs $ getProb flipThree'
    where
        flipThree' = do
            a <- coin
            b <- coin
            c <- loadedCoin
            return (all (==Tails) [a, b, c])
