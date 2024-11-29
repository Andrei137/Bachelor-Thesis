{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Do.Set3 where
import MCPrelude
import Types
import Set5

-- Generating combinations
allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = do
    a <- as
    b <- bs
    return (a, b)

-- Poker hands
allCards :: [Int] -> [String] -> [Card]
allCards is ss = do
    i <- is
    s <- ss
    return $ Card i s

-- Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = do
    a <- as
    b <- bs
    return $ f a b

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = do
    a <- as
    b <- bs
    c <- cs
    return $ f a b c

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f as bs cs ds = do
    a <- as
    b <- bs
    c <- cs
    d <- ds
    return $ f a b c d

-- Verifying
verifyAllPairs :: Bool
verifyAllPairs =
    allPairs [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)] &&
    allPairs [1..3] [6..8] == [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]

verifyAllCards :: Bool
verifyAllCards = show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"

verifyAllCombs3 :: Bool
verifyAllCombs3 = allCombs3 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
