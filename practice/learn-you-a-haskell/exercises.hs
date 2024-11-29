import Data.List
import Data.Char

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip' = zipWith' (\x y -> (x, y))

filter' :: [(a -> Bool)] -> [a] -> [a]
filter' [] x = x
filter' (f:fx) x = filter' fx (filter f x)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge l1@(x:xs) l2@(y:ys)
    | x < y = x : merge xs l2
    | otherwise = y : merge l1 ys

mergesort :: (Ord a) => [a] -> [a]
mergesort arr = mergesort' arr 0 n
    where
        n = length arr
        mergesort' arr l r
            | l >= r = [arr !! l]
            | otherwise = merge (mergesort' arr l m) (mergesort' arr (m + 1) r)
            where m = (l + r) `div` 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let 
        smaller = filter (<= x) xs
        bigger = filter (> x) xs
    in 
        quicksort smaller ++ [x] ++ quicksort bigger

assureCollatz :: Int -> Bool
assureCollatz n = foldr (&&) True . filter (\x -> x == False) . map (\xs -> last xs == 1) $ map chain [1..n]
    where chain n
            | even n = n : chain (n `div` 2)
            | n == 1 = [1]
            | otherwise = n : chain (n * 3 + 1)

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any' (needle `isPrefixOf'`) (tails' haystack)
    where
        any' :: (a -> Bool) -> [a] -> Bool
        any' _ [] = False
        any' f (x:xs)
            | f x == True = True
            | otherwise = any' f xs

        isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
        isPrefixOf' [] _ = True
        isPrefixOf' _ [] = False
        isPrefixOf' (x:xs) (y:ys)
            | x /= y = False
            | otherwise = isPrefixOf' xs ys

        tails' :: [a] -> [[a]]
        tails' [] = [[]]
        tails' l@(_:xs) = l : tails' xs

firstTo :: Int -> Maybe Integer
firstTo n = find (\x -> sumCif x == n) [1..]
    where
        sumCif :: Integer -> Int
        sumCif = sum . map digitToInt . show

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [
    ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
data TrafficLight = Red | Yellow | Green

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | otherwise = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | otherwise = treeElem x right

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = True
    yesno _ = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesNoVal yesResult noResult
    | yesno yesNoVal = yesResult
    | otherwise = noResult
