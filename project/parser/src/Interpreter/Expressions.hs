{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter.Expressions
    ( interpretExpr
    ) where

import GHC.Base (when)
import Control.Monad (forM, forM_)
import AST.Types
import AST.Operators
import AST.Expressions
import Interpreter.Core
import Interpreter.Operators

unaryFuncDbl :: String -> (Double -> Double) -> [Expr] -> Interpreter Value
unaryFuncDbl name f params = do
    when (length params /= 1) $
        error $ name ++ " function expects exactly one parameter"
    [param] <- mapM interpretExpr params
    case param of
        IntVal i -> return $ DoubleVal $ f $ fromIntegral i
        DoubleVal d -> return $ DoubleVal $ f d
        _ -> error "Type error, expected integer or double"

unaryFuncInt :: String -> (Double -> Integer) -> [Expr] -> Interpreter Value
unaryFuncInt name f params = do
    when (length params /= 1) $
        error $ name ++ " function expects exactly one parameter"
    [param] <- mapM interpretExpr params
    case param of
        IntVal i -> return $ IntVal $ f $ fromIntegral i
        DoubleVal d -> return $ IntVal $ f d
        _ -> error "Type error, expected integer or double"

anyFunc :: String -> ([Double] -> Double) -> [Expr] -> Interpreter Value
anyFunc name f params = do
    values <- mapM interpretExpr params
    let allInts = all isInt values
    nums <- forM values $ \case
        IntVal i -> return $ fromIntegral i
        DoubleVal d -> return d
        _ -> error $ name ++ " function expects only numeric parameters"
    let result = f nums
    return $ if allInts then IntVal (round result) else DoubleVal result
    where
        isInt (IntVal _) = True
        isInt _ = False

defaultValue :: Type -> Value
defaultValue IntT = IntVal 0
defaultValue DoubleT = DoubleVal 0.0
defaultValue BoolT = BoolVal False
defaultValue CharT = CharVal '\0'
defaultValue StringT = StringVal ""
defaultValue _ = error "Unsupported type for default value"

interpretExpr :: Expr -> Interpreter Value
interpretExpr (Var var) = find var
interpretExpr (FuncCall func params) = case func of
    "sqrt" -> unaryFuncDbl func sqrt params
    "log" -> unaryFuncDbl func log params
    "floor" -> unaryFuncInt func floor params
    "ceil" -> unaryFuncInt func ceiling params
    "abs" -> unaryFuncDbl func abs params
    "max" -> anyFunc func maximum params
    "min" -> anyFunc func minimum params
    _ -> error $ "Function not implemented: " ++ func
interpretExpr (IntConst i) = return $ IntVal i
interpretExpr (DoubleConst d) = return $ DoubleVal d
interpretExpr (BoolConst b) = return $ BoolVal b
interpretExpr (CharConst c) = return $ CharVal c
interpretExpr (StringConst s) = return $ StringVal s
interpretExpr (UnaryOp op expr) = do
    value <- interpretExpr expr
    return $ interpretUnaryOp op value
interpretExpr (BinaryOp op expr1 expr2) = do
    value1 <- interpretExpr expr1
    value2 <- interpretExpr expr2
    return $ interpretBinaryOp op value1 value2
interpretExpr (Declare typ vars) = do
    forM_ vars $ \(name, maybeExpr) ->
        ( do case maybeExpr of
                Nothing -> set name $ defaultValue typ
                Just expr -> do
                    value <- interpretExpr expr
                    case value of
                        IntVal _ | typ == IntT ->
                            set name value
                        IntVal i | typ == DoubleT ->
                            set name $ DoubleVal $ fromIntegral i
                        DoubleVal _ | typ == DoubleT ->
                            set name value
                        DoubleVal d | typ == IntT ->
                            set name $ IntVal $ round d
                        BoolVal _ | typ == BoolT ->
                            set name value
                        CharVal _ | typ == CharT ->
                            set name value
                        StringVal _ | typ == StringT ->
                            set name value
                        _ -> error "Type error in declaration"
        )
    return VoidVal
interpretExpr (Assign var op expr) = do
    value <- interpretExpr expr
    case op of
        Basic -> do
            mOldValue <- optionalFind var
            case mOldValue of
                Just oldValue ->
                    if typeMatches oldValue value
                        then set var value
                        else error "Type mismatch in assignment"
                Nothing -> set var value
            set var value
        (With binaryOp) -> do
            oldValue <- find var
            let newValue = interpretBinaryOp binaryOp oldValue value
            if typeMatches oldValue newValue
                then set var newValue
                else error "Type mismatch in assignment"
    return VoidVal
