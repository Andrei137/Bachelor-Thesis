{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Initial.Set3 where
import MCPrelude
import Types

-- Generating combinations
allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

-- Poker hands
allCards :: [Int] -> [String] -> [Card]
allCards = allCombs (\x y -> Card x y)

-- Generalizing pairs and cards
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f xs ys = combStep (map f xs) ys

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = combStep (combStep (map f xs) ys) zs

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f xs ys zs ts = combStep (combStep (combStep (map f xs) ys) zs) ts

-- Combinations of more things
combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) xs = (map f xs) ++ (combStep fs xs)

-- Verifying
verifyAllPairs :: Bool
verifyAllPairs =
    allPairs [1,2] [3,4] == [(1,3),(1,4),(2,3),(2,4)] &&
    allPairs [1..3] [6..8] == [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]

verifyAllCards :: Bool
verifyAllCards = show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"

verifyAllCombs3 :: Bool
verifyAllCombs3 = allCombs3 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
