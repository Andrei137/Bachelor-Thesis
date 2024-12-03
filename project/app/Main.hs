module Main (main) where

import Parser
import Helpers
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let pretty = if length args > 0 then args !! 0 else "Off"
        action = if pretty == "On" then prettify else show
        txtFolder = "bin/txt/"
        inputFile = txtFolder ++ "example.in"
        astFile = txtFolder ++ "ast.txt"
        outputFile = txtFolder ++ "example.out"
    ast <- parseFile inputFile
    putStrLn "AST built successfully!"
    writeFile astFile $ action ast
    writeFile outputFile $ reconstruct ast
