module Main (main) where

import System.Environment (getArgs)
import Utils

main :: IO ()
main = do
    args <- getArgs
    case reverse args of
        [arg] ->
            if arg == "expose"
                then runApp 8080
                else processFile arg [Run]
        (file:actions) -> do
            case mapM readAction (reverse actions) of
                Just actions' -> processFile file actions'
                Nothing -> putStrLn "Error: Invalid action"
        _ -> putStrLn "Usage: surge [expose | ast/prettify/run <input_file>]"
