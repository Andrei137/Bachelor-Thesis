import System.IO
import Data.Char 

main = do
    content <- readFile "txt/girlfriend.txt"
    writeFile "txt/girlfriendCaps.txt" (map toUpper content)

