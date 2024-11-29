type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

flyAwayLeft :: Birds -> Pole -> Maybe Pole
flyAwayLeft n = landLeft (-n)

flyAwayRight :: Birds -> Pole -> Maybe Pole
flyAwayRight n = landRight (-n)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

landLeft' :: Birds -> Pole -> Either String Pole
landLeft' n (left,right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise = Left $ "Failure: " ++ show (left + n) ++ " | " ++ show right

landRight' :: Birds -> Pole -> Either String Pole
landRight' n (left,right)
    | abs (left - (right + n)) < 4 = Right (left, right+n)
    | otherwise = Left $ "Failure: " ++ show left ++ " | " ++ show (right + n)

flyAwayLeft' :: Birds -> Pole -> Either String Pole
flyAwayLeft' n = landLeft' (-n)

flyAwayRight' :: Birds -> Pole -> Either String Pole
flyAwayRight' n = landRight' (-n)

routine' :: Either String Pole
routine' = return (0,0) >>= landLeft' 2 >>= landRight' 2 >>= landLeft' 10
