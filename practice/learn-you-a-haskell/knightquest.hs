import Data.Maybe
import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c-1,r-2),(c+1,r-2),(c-2,r-1),(c-2,r+1),
                 (c-1,r+2),(c+1,r+2),(c+2,r-1),(c+2,r+1)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

movePath :: [KnightPos] -> [[KnightPos]]
movePath path = do
    next <- moveKnight (head path)
    return (next : path)

in3Path :: KnightPos -> [[KnightPos]]
in3Path start = return [start] >>= movePath >>= movePath >>= movePath

canReachIn3 :: KnightPos -> KnightPos -> Maybe [KnightPos]
canReachIn3 start end =
    case filter ((== end) . head) $ in3Path start of
        (path:_) -> Just (reverse path)
        [] -> Nothing

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

inManyPath :: Int -> KnightPos -> [[KnightPos]]
inManyPath x start = return [start] >>= foldr (<=<) return (replicate x movePath)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

pathReachIn :: Int -> KnightPos -> KnightPos -> Maybe [KnightPos]
pathReachIn x start end =
    case filter ((==end) . head) $ inManyPath x start of
        (path:_) -> Just (reverse path)
        [] -> Nothing

tryToReach' :: Int -> KnightPos -> KnightPos -> Maybe [KnightPos]
tryToReach' 10 start end = pathReachIn 10 start end
tryToReach' n start end =
    case pathReachIn n start end of
        Just path -> Just path
        Nothing -> tryToReach' (n + 1) start end

tryToReach :: KnightPos -> KnightPos -> Maybe [KnightPos]
tryToReach = tryToReach' 0

tryToReach'' :: KnightPos -> KnightPos -> Maybe [KnightPos]
tryToReach'' start end = listToMaybe $ mapMaybe (\i -> pathReachIn i start end) [1..10]
