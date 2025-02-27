module Interpreter.Expressions
    ( interpretAExpr
    , interpretBExpr
    ) where

import AST.Types.Expressions
import Interpreter.Core
import Interpreter.Operators

interpretAExpr :: AExpr -> Interpreter Value
-- Var String
interpretAExpr (Var var) = find var
-- IntConst Integer
interpretAExpr (IntConst i) = return $ IntVal i
-- AUnary AUnOp AExpr
interpretAExpr (AUnary op expr) = do
    value <- interpretAExpr expr
    case value of
        IntVal i -> return $ IntVal $ interpretAUnOp op i
        _ -> error "Type error, expected integer"
-- ABinary ABinOp AExpr AExpr
interpretAExpr (ABinary op expr1 expr2) = do
    value1 <- interpretAExpr expr1
    value2 <- interpretAExpr expr2
    case (value1, value2) of
        (IntVal i1, IntVal i2) -> return $ IntVal $ interpretABinOp op i1 i2
        _ -> error "Type error, expected integers"
-- AFuncCall String [AExpr]
interpretAExpr (AFuncCall func args) = do
    values <- mapM interpretAExpr args
    case func of
        "sqrt" -> do
            case values of
                [IntVal i] ->
                    let result = floor (sqrt (fromIntegral i :: Double)) :: Integer
                    in return $ IntVal result
                _ -> error "Type error, expected integer"
        _ -> error "Function not found"

interpretBExpr :: BExpr -> Interpreter Value
-- BoolConst Bool
interpretBExpr (BoolConst b) = return $ BoolVal b
-- BUnary BUnOp BExpr
interpretBExpr (BUnary op expr) = do
    value <- interpretBExpr expr
    case value of
        BoolVal b -> return $ BoolVal $ interpretBUnOp op b
        _ -> error "Type error, expected boolean"
-- BBinary BBinOp BExpr BExpr
interpretBExpr (BBinary op expr1 expr2) = do
    value1 <- interpretBExpr expr1
    value2 <- interpretBExpr expr2
    case (value1, value2) of
        (BoolVal b1, BoolVal b2) -> return $ BoolVal $ interpretBBinOp op b1 b2
        _ -> error "Type error, expected booleans"
-- RBinary RBinOp AExpr AExpr
interpretBExpr (RBinary op expr1 expr2) = do
    value1 <- interpretAExpr expr1
    value2 <- interpretAExpr expr2
    case (value1, value2) of
        (IntVal i1, IntVal i2) -> return $ BoolVal $ interpretRBinOp op i1 i2
        _ -> error "Type error, expected integers"
