-- 1
myLast :: [a] -> a
myLast [] = error "List must not be empty"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "List must have at least two elements"
myButLast [x] = error "List must have at least two elements"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index too big"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs $ n - 1

-- 4
myLength :: [a] -> Int
myLength = foldl (const . (+1)) 0

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = ls == foldl (flip (:)) [] ls

-- 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = foldl (\acc x -> acc ++ myFlatten x) [] xs

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = x:compress (y:xs)

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x == head gx then (x:gx):gxs else [x]:gx:gxs
    where (gx:gxs) = pack xs

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode ls = foldr (\l acc -> (length l, head l):acc) [] $ pack ls

-- 11
data EncodedItem a = Single a | Multiple Int a deriving Show
type Encoded a = [EncodedItem a]

encodeModified :: Eq a => [a] -> Encoded a
encodeModified ls = foldr f [] $ pack ls
    where f l acc
            | length l == 1 = (Single $ head l):acc
            | otherwise = (Multiple (length l) (head l)):acc

-- 12
decodeModified :: Encoded a -> [a]
decodeModified = foldr f []
    where f ex acc = case ex of
            Single x -> x:acc
            Multiple n x -> (replicate n x) ++ acc

-- 13
encodeElement 1 = Single
encodeElement n = Multiple n

encodeDirect :: (Eq a) => [a] -> Encoded a
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs
    where encodeDirect' n y [] = [encodeElement n y]
          encodeDirect' n y (x:xs)
            | y == x    = encodeDirect' (n+1) y xs
            | otherwise = encodeElement n y :(encodeDirect' 1 x xs)

-- 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = repli' x n ++ repli xs n
    where repli' _ 0 = []
          repli' x n = x:repli' x (n-1)

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd . filter (\(idx, _) -> idx `mod` n /= 0) $ zip [1..] xs

-- 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split l@(x:xs) n
    | n > 0 = (x:l1, l2)
    | otherwise = ([], l)
    where (l1, l2) = split xs (n - 1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice = slice' 1
    where slice' _ [] _ _ = []
          slice' i (x:xs) l r
            | i >= l && i <= r = x:slice' (i + 1) xs l r
            | otherwise = slice' (i + 1) xs l r

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n
    | n < 0 = rotate xs $ length xs + n
    | otherwise =  drop n xs ++ take n xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Index too big"
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = (l, x:r)
    where (l, r) = removeAt (n - 1) xs
