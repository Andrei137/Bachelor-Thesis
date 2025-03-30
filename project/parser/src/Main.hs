module Main (main) where

import System.Environment (getArgs)
import Utils

main :: IO ()
main = do
    args <- getArgs
    case reverse args of
        [file] -> processFile file [Run]
        (file:actions) -> do
            case mapM readAction (reverse actions) of
                Just actions' -> processFile file actions'
                Nothing -> putStrLn "Error: Invalid action"
        _ -> putStrLn "Usage: surge [run|ast|prettify] <input_file>"
