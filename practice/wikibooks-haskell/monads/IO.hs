module IO where

import Data.Char (toUpper)
import Control.Monad

main = do
    string <- putStrLn "Write your string: " *> getLine
    putStrLn $ shout string

shout = map toUpper

speakTo :: (String -> String) -> IO String
speakTo fSentence = fmap fSentence getLine

sayHello :: IO String
sayHello = speakTo (\name -> "Hello, " ++ name ++ "!")

fiveGetLines :: IO [String]
{- fiveGetLines = sequence $ replicate 5 getLine -}
fiveGetLines = replicateM 5 getLine

printMany :: Show a => [a] -> IO ()
printMany = mapM_ print

generation :: a -> [a]
generation = replicate 3

bunnyInvasion :: Int -> [String]
bunnyInvasion = invasion ["bunny"]
    where
    invasion animals 0 = animals
    invasion animals n = do
        animal <- animals
        invasion (generation animal) (n - 1)
