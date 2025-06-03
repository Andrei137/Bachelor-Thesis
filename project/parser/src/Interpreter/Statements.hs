{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Interpreter.Statements
    ( interpretAST
    ) where

import Control.Monad
import Data.Char (toLower)
import Data.List.Split (splitOn)
import System.IO (hFlush, stdout)
import Control.Monad.State (liftIO)
import AST.Types
import AST.Operators
import AST.Statements
import Interpreter.Core
import Interpreter.Operators
import Interpreter.Expressions

defaultValue :: Type -> Value
defaultValue IntT = IntVal 0
defaultValue DoubleT = DoubleVal 0.0
defaultValue BoolT = BoolVal False
defaultValue CharT = CharVal '\0'
defaultValue StringT = StringVal ""
defaultValue _ = error "Unsupported type for default value"

interpretStmt :: Stmt -> Interpreter ()
interpretStmt (OneComm _) = return ()
interpretStmt (MultiComm _) = return ()
interpretStmt (Expr expr) = void $ interpretExpr expr
interpretStmt (Read var) = do
    varType <- find var
    liftIO $ putStr (var ++ " = ")
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case varType of
        IntVal _ -> case (reads input :: [(Integer, String)]) of
            [(value, "")] -> set var (IntVal value)
            _ -> error $ var ++ " is int, got " ++ input
        DoubleVal _ -> case (reads input :: [(Double, String)]) of
            [(value, "")] -> set var (DoubleVal value)
            _ -> error $ var ++ " is double, got " ++ input
        BoolVal _ -> case map toLower input of
            "true" -> set var (BoolVal True)
            "false" -> set var (BoolVal False)
            _ -> error $ var ++ " is bool, got " ++ input
        CharVal _ -> case input of
            [c] -> set var (CharVal c)
            _ -> error $ var ++ " is char, got " ++ input
        StringVal _ -> set var (StringVal input)
interpretStmt (Print expr) = do
    expr' <- interpretExpr expr
    case expr' of
        StringVal str -> do
            let parts = splitOn "${" str
            case parts of
                [] -> liftIO $ putStrLn str
                (first:rest) -> do
                    liftIO $ putStr first
                    mapM_ (\part -> do
                        let (var:rest') = splitOn "}" part
                        value <- find var
                        liftIO $ putStr (showValue value ++ concat rest')
                        ) rest
                    liftIO $ putStrLn ""
        _ -> liftIO $ putStrLn (showValue expr')
interpretStmt (Declare (Decl typ vars)) = do
    forM_ vars $ \(name, maybeExpr) -> do
        case maybeExpr of
            Nothing -> set name $ defaultValue typ
            Just expr -> do
                value <- interpretExpr expr
                case value of
                    IntVal _ | typ == IntT -> set name value
                    DoubleVal _ | typ == DoubleT -> set name value
                    BoolVal _ | typ == BoolT -> set name value
                    CharVal _ | typ == CharT -> set name value
                    StringVal _ | typ == StringT -> set name value
                    _ -> error "Type error in declaration"
interpretStmt (Assign var op expr) = do
    value <- interpretExpr expr
    case op of
        Basic -> set var value
        (With binaryOp) -> do
            oldValue <- find var
            case (oldValue, value) of
                (IntVal i1, IntVal i2) -> set var $ IntVal (interpretABinOp binaryOp i1 i2)
                _ -> error "Type error, expected integer"
interpretStmt (If cond stmt1 Nothing) = do
    value <- interpretExpr cond
    case value of
        BoolVal True -> interpretStmt stmt1
        BoolVal False -> return ()
        _ -> error "Type error, expected boolean"
interpretStmt (If cond stmt1 (Just stmt2)) = do
    value <- interpretExpr cond
    case value of
        BoolVal True -> interpretStmt stmt1
        BoolVal False -> interpretStmt stmt2
        _ -> error "Type error, expected boolean"
interpretStmt (While cond stmt) = do
    value <- interpretExpr cond
    case value of
        BoolVal True -> interpretStmt stmt >> interpretStmt (While cond stmt)
        BoolVal False -> return ()
        _ -> error "Type error, expected boolean"
interpretStmt (For initial cond update stmt) = do
    interpretStmt initial
    interpretStmt (While cond (Seq [stmt, update]))
interpretStmt (Seq stmts) = mapM_ interpretStmt stmts
interpretStmt _ = error "Not handled yet"

interpretAST :: Stmt -> IO ()
interpretAST = interpretWith interpretStmt
