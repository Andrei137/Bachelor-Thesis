module Utils
    ( Action(..)
    , readAction
    , processFile
    ) where

import Data.Char (toLower)
import System.FilePath (takeBaseName, takeExtension)
import AST.Types.Statements (Stmt(..))
import Parser.Statements (parseAST)
import Prettifier.Statements (prettifyAST)
import AST.Formatter.Statements (formatAST)
import Interpreter.Statements (interpretAST)

data Action
    = Run
    | Ast
    | Prettify
    | Full
    deriving (Show, Read)

readAction :: String -> Maybe Action
readAction actionStr = case map toLower actionStr of
    "run"      -> Just Run
    "ast"      -> Just Ast
    "prettify" -> Just Prettify
    "full"     -> Just Full
    _          -> Nothing

performAction :: FilePath -> Stmt -> Action -> IO ()
performAction inputFile ast action = case action of
    Run -> interpretAST ast
    Ast -> do
        let baseName = takeBaseName inputFile
        writeFile (baseName ++ "_AST.txt") $ formatAST ast
        putStrLn "AST generated successfully"
    Prettify -> do
        let baseName = takeBaseName inputFile
        writeFile (baseName ++ "_bkp.sg") =<< readFile inputFile
        writeFile inputFile $ prettifyAST ast
        putStrLn "Code prettified successfully"
    Full -> do
        mapM_ (performAction inputFile ast) [Run, Prettify, Ast]

processFile :: FilePath -> [Action] -> IO ()
processFile inputFile actions
    | takeExtension inputFile /= ".sg" = putStrLn "Error: Invalid file extension"
    | otherwise = do
        program <- readFile inputFile
        case parseAST program of
            Left e -> print e >> fail "parse error"
            Right ast -> mapM_ (performAction inputFile ast) actions
