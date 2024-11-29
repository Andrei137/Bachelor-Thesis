{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Initial.Set1 where
import MCPrelude

-- Helpers
mySeed :: Seed
mySeed = mkSeed 1

-- Random Number Generation
getRandIntegers :: Int -> Seed -> [Integer]
getRandIntegers 0 _ = []
getRandIntegers n seed =
    let (integer, nextSeed) = rand seed
    in integer: getRandIntegers (n - 1) nextSeed

fiveRands :: [Integer]
fiveRands = getRandIntegers 5 mySeed

-- Random Character Generation
randLetter :: Gen Char
randLetter seed =
    let (integer, nextSeed) = rand seed
    in (toLetter integer, nextSeed)

getRandChars :: Int -> Seed -> String
getRandChars 0 _ = []
getRandChars n seed =
    let (char, nextSeed) = randLetter seed
    in char: getRandChars (n - 1) nextSeed

randString3 :: String
randString3 = getRandChars 3 mySeed

-- More Generators
type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*5) randEven

generalA :: (a -> b) -> Gen a -> Gen b
generalA f randA seed =
    let (nextA, nextSeed) = randA seed
    in (f nextA, nextSeed)

-- Generalizing Random Pairs
randPair :: Gen (Char, Integer)
randPair seed =
    let
        (nextChar, nextSeed1) = randLetter seed
        (nextInteger, nextSeed2) = rand nextSeed1
    in ((nextChar, nextInteger), nextSeed2)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair randA randB seed =
    let
        (nextA, nextSeed1) = randA seed
        (nextB, nextSeed2) = randB nextSeed1
    in ((nextA, nextB), nextSeed2)

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f randA randB seed =
    let
        (nextA, nextSeed1) = randA seed
        (nextB, nextSeed2) = randB nextSeed1
    in (f nextA nextB, nextSeed2)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (\myChar myInteger -> (myChar, myInteger))

-- Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([], seed)
repRandom (x:xs) seed =
    let
        (result, nextSeed) = x seed
        (myTail, finalSeed) = repRandom xs nextSeed
    in (result: myTail, finalSeed)

-- Threading the random number state
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo randA f seed =
    let
        (nextA, nextSeed1) = randA seed
    in f nextA nextSeed1

mkGen :: a -> Gen a
mkGen value seed = (value, seed)

-- A Missed Generalization
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f randA randB =
    randA `genTwo` \a ->
    randB `genTwo` \b ->
    mkGen (f a b)

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 = foldr f (mkGen [])
    where
        f randA xs =
            randA `genTwo` \a ->
            generalA (a:) xs

-- Verifying
verifyFiveRands :: Bool
verifyFiveRands = product fiveRands == 8681089573064486461641871805074254223660

verifyRandString3 :: Bool
verifyRandString3 = randString3 == "lrf"

verifyGeneralA :: Bool
verifyGeneralA =
    (myEven * myOdd * myTen) == 189908109902700
    where
        myEven = fst $ randEven mySeed
        myOdd = fst $ randOdd mySeed
        myTen = fst $ randTen mySeed

verifyRandPair :: Bool
verifyRandPair =
    let (result, _) = randPair mySeed
    in result == ('l',282475249)

verifyRandPair_ :: Bool
verifyRandPair_ =
    let
        (result, _) = randPair mySeed
        (result_, _) = randPair_ mySeed
    in result == result_

verifyRepRandom :: Bool
verifyRepRandom =
    let (result, _) = (repRandom (replicate 3 randLetter) mySeed)
    in result == randString3

verifyRepRandom2 :: Bool
verifyRepRandom2 =
    let (result, _) = (repRandom2 (replicate 3 randLetter) mySeed)
    in result == randString3
