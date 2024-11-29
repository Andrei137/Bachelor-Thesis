{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Do.Set1 where
import MCPrelude
import Types
import Set5

-- Helpers
mySeed :: Seed
mySeed = mkSeed 1

randGen :: Gen Integer
randGen = Gen rand

myEvalGen :: Gen a -> a
myEvalGen g = evalGen g mySeed

-- Random Number Generation
fiveRands :: Gen [Integer]
fiveRands = do
    r1 <- randGen
    r2 <- randGen
    r3 <- randGen
    r4 <- randGen
    r5 <- randGen
    return [r1, r2, r3, r4, r5]

-- Random Character Generation
randLetter :: Gen Char
randLetter = do
    value <- randGen
    return (toLetter value)

randString3 :: Gen String
randString3 = do
    r1 <- randLetter
    r2 <- randLetter
    r3 <- randLetter
    return [r1, r2, r3]

-- More Generators
randEven :: Gen Integer
randEven = generalA (*2) randGen

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*5) randEven

generalA :: (a -> b) -> Gen a -> Gen b
generalA f a = Gen $ \seed ->
    let (value, nextSeed) = runGen a seed
    in (f value, nextSeed)

-- Generalizing Random Pairs
randPair :: Gen (Char, Integer)
randPair =
    randLetter >>= \myChar ->
    randGen >>= \myInteger ->
    return (myChar, myInteger)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair a b = do
    a' <- a
    b' <- b
    return (a', b')

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter randGen

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f a b =
    a >>= \a' ->
    b >>= \b' ->
    return (f a' b')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

-- Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

-- Threading the random number state
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = (>>=)

mkGen :: a -> Gen a
mkGen = return

-- A Missed Generalization
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 = liftM2

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 = sequence

-- Verifying
verifyFiveRands :: Bool
verifyFiveRands = (product $ myEvalGen fiveRands) == 8681089573064486461641871805074254223660

verifyRandString3 :: Bool
verifyRandString3 = (myEvalGen randString3) == "lrf"

verifyGeneralA :: Bool
verifyGeneralA =
    (myEven * myodd * myTen) == 189908109902700
    where
        myEven = myEvalGen randEven
        myodd = myEvalGen randOdd
        myTen = myEvalGen randTen

verifyRandPair :: Bool
verifyRandPair = myEvalGen randPair == ('l',282475249)

verifyRandPair_ :: Bool
verifyRandPair_ = myEvalGen randPair == myEvalGen randPair_

verifyRepRandom :: Bool
verifyRepRandom = (myEvalGen . repRandom $ replicate 3 randLetter) == (myEvalGen randString3)

verifyRepRandom2 :: Bool
verifyRepRandom2 = (myEvalGen . repRandom2 $ replicate 3 randLetter) == (myEvalGen randString3)
