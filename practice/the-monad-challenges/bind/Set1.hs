{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Bind.Set1 where
import MCPrelude
import Types
import Set4

-- Helpers
mySeed :: Seed
mySeed = mkSeed 1

randGen :: Gen Integer
randGen = Gen rand

myEvalGen :: Gen a -> a
myEvalGen g = evalGen g mySeed

-- Random Number Generation
fiveRands :: [Integer]
fiveRands = generateN randGen 5

-- Random Character Generation
randLetter :: Gen Char
randLetter =
    (Gen rand) `bind` \value ->
    return (toLetter value)

randString3 :: String
randString3 = generateN randLetter 3

generateN :: Gen a -> Int -> [a]
generateN randF n = fst $ runGen (sequence . replicate n $ randF) mySeed

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
    randLetter `bind` \myChar ->
    randGen `bind` \myInteger ->
    return (myChar, myInteger)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair a b =
    a `bind` \a' ->
    b `bind` \b' ->
    return (a', b')

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter randGen

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f a b =
    a `bind` \a' ->
    b `bind` \b' ->
    return (f a' b')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

-- Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

-- Threading the random number state
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

mkGen :: a -> Gen a
mkGen = return

-- A Missed Generalization
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 = liftM2

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 = sequence

-- Verifying
verifyFiveRands :: Bool
verifyFiveRands = product fiveRands == 8681089573064486461641871805074254223660

verifyRandString3 :: Bool
verifyRandString3 = randString3 == "lrf"

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
verifyRepRandom = (myEvalGen . repRandom $ replicate 3 randLetter) == randString3

verifyRepRandom2 :: Bool
verifyRepRandom2 = (myEvalGen . repRandom2 $ replicate 3 randLetter) == randString3
