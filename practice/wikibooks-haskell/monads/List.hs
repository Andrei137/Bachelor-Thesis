generation :: a -> [a]
generation = replicate 3

themselvesTimes :: [Int] -> [Int]
themselvesTimes xs = do
    x <- xs
    replicate x x
