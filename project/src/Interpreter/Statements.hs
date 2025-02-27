{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Interpreter.Statements
    ( interpretAST
    ) where

import Data.List.Split (splitOn)
import System.IO (hFlush, stdout)
import Control.Monad.State (liftIO)
import AST.Types.Operators
import AST.Types.Statements
import Interpreter.Core
import Interpreter.Operators
import Interpreter.Expressions

interpretStmt :: Stmt -> Interpreter ()
-- Skip
interpretStmt Skip = return ()
-- Read String
interpretStmt (Read var) = do
    liftIO $ putStr (var ++ " = ")
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case reads input of
        [(i, "")] -> set var (IntVal i)
        _ -> error "Type error, expected integer"
-- Print AExpr
interpretStmt (Print expr) = do
    case expr of
        String str -> do
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
        AExpr expr' -> do
            value <- interpretAExpr expr'
            case value of
                IntVal i -> liftIO $ print i
                BoolVal b -> liftIO $ print b
-- Assign String AExpr
interpretStmt (Assign op var expr) = do
    value <- interpretAExpr expr
    case op of
        Basic -> set var value
        (With binaryOp) -> do
            oldValue <- find var
            case (oldValue, value) of
                (IntVal i1, IntVal i2) -> set var $ IntVal (interpretABinOp binaryOp i1 i2)
                _ -> error "Type error, expected integer"
-- If BExpr Stmt Stmt
interpretStmt (If cond stmt1 stmt2) = do
    value <- interpretBExpr cond
    case value of
        BoolVal True -> interpretStmt stmt1
        BoolVal False -> interpretStmt stmt2
        _ -> error "Type error, expected boolean"
-- While BExpr Stmt
interpretStmt (While cond stmt) = do
    value <- interpretBExpr cond
    case value of
        BoolVal True -> interpretStmt stmt >> interpretStmt (While cond stmt)
        BoolVal False -> return ()
        _ -> error "Type error, expected boolean"
-- For Stmt BExpr Stmt Stmt
interpretStmt (For initial cond update stmt) = do
    interpretStmt initial
    interpretStmt (While cond (Seq [stmt, update]))
-- Seq [Stmt]
interpretStmt (Seq stmts) = mapM_ interpretStmt stmts

interpretAST :: Stmt -> IO ()
interpretAST = interpretWith interpretStmt
