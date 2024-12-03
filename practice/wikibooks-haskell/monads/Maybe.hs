import Data.Maybe (fromMaybe)
import Control.Monad ((<=<))

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0     = Just $ log x
    | otherwise = Nothing

safeDiv :: (Fractional a, Eq a) => a -> a -> Maybe a
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just $ x / y

safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt x
    | x >= 0    = Just $ sqrt x
    | otherwise = Nothing

safeArcSin :: (Floating a, Ord a) => a -> Maybe a
safeArcSin x
    | x >= -1 && x <= 1 = Just $ asin x
    | otherwise = Nothing

safeLogSqrt :: (Floating a, Ord a) => a -> Maybe a
safeLogSqrt = safeLog <=< safeSqrt
{- unsafeLogSqrt = log . sqrt -}

phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"),
              ("Fred",  "01624 556442"),
              ("Alice", "01889 985333"),
              ("Jane",  "01732 187565") ]

governmentDatabase :: [(String, String)]
governmentDatabase = [ ("01788 665242", "Romania"),
                       ("01624 556442", "USA"),
                       ("01889 985333", "Sweden"),
                       ("01732 187565", "France") ]

taxDatabase :: [(String, Double)]
taxDatabase = [ ("Romania", 0.17),
                ("USA", 0.15),
                ("Sweden", 0.22),
                ("France", 0.19) ]

getRegistrationNumber :: String -> Maybe String
getRegistrationNumber name =
    lookup name phonebook >>=
        (\number -> lookup number governmentDatabase)

getTaxOwed :: String -> Maybe Double
getTaxOwed name = do
    number <- lookup name phonebook
    registration <- lookup number governmentDatabase
    lookup registration taxDatabase

zeroAsDefault :: Maybe Int -> Int
zeroAsDefault = fromMaybe 0

displayResult :: Maybe Int -> String
displayResult = maybe "There was no result" $ ("The result was " ++) . show
