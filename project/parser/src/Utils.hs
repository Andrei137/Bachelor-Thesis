{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( Action(..)
    , readAction
    , processFile
    , runApp
    ) where

import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(..), Indent(Spaces))
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.Char (toLower)
import System.FilePath (takeBaseName, takeExtension)
import AST.Statements (Stmt(..))
import Parser.Statements (parseAST)
import Prettifier.Statements (prettifyAST)
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

app :: Application
app req respond = do
    case (requestMethod req, pathInfo req) of
        ("POST", ["ast"]) -> do
            body <- strictRequestBody req
            case parseAST (TL.unpack $ TLE.decodeUtf8 body) of
                Left err -> respond $ responseLBS status400 [("Content-Type", "text/plain")] (BL.pack $ show err)
                Right ast -> do
                    let jsonAst = Pretty.encodePretty ast
                    respond $ responseLBS status200 [("Content-Type", "application/json")] jsonAst
        _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "Invalid endpoint"

runApp :: Int -> IO ()
runApp port = do
    putStrLn $ "Running server on port " ++ show port
    run port app

performAction :: FilePath -> Stmt -> Action -> IO ()
performAction inputFile ast action = case action of
    Run -> interpretAST ast
    Ast -> do
        let baseName = takeBaseName inputFile
        writeFile (baseName ++ ".json") $ BL.unpack . encodePretty' (defConfig { confIndent = Spaces 2 }) $ ast
        putStrLn "JSON generated successfully"
    Prettify -> do
        let baseName = takeBaseName inputFile
        writeFile (baseName ++ ".bak") =<< readFile inputFile
        writeFile inputFile $ prettifyAST ast
        putStrLn "Code prettified successfully"
    Full -> do
        mapM_ (performAction inputFile ast) [Run, Prettify, Ast]

processFile :: FilePath -> [Action] -> IO ()
processFile inputFile actions
    | takeExtension inputFile /= ".sg" = putStrLn "Error: Invalid file extension, expected .sg"
    | otherwise = do
        program <- readFile inputFile
        case parseAST program of
            Left e -> print e >> fail "parse error"
            Right ast -> mapM_ (performAction inputFile ast) actions
