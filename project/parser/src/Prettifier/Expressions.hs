module Prettifier.Expressions
    ( convertExpr
    ) where

import Data.List (intercalate)
import Prettifier.Types
import Prettifier.Operators
import AST.Expressions

convertExpr :: Expr -> String
convertExpr (Var var) = var
convertExpr (FuncCall name params) = name ++ "(" ++ intercalate ", " (map convertExpr params) ++ ")"
convertExpr (IntConst i) = show i
convertExpr (DoubleConst d) = show d
convertExpr (BoolConst b) = show b
convertExpr (CharConst c) = show c
convertExpr (StringConst s) = show s
convertExpr (UnaryOp op expr) = convertUnaryOp op (convertExpr expr)
convertExpr (BinaryOp op expr1 expr2) = convertBinaryOp op (convertExpr expr1) (convertExpr expr2)
convertExpr (Declare typ vars) =
    convertType typ ++ " " ++ intercalate ", " (map convertVar vars)
  where
    convertVar (name, Nothing) = name
    convertVar (name, Just expr) = name ++ "{ " ++ convertExpr expr ++ " }"
convertExpr (Assign var op expr) = var ++ convertAssignOp op ++ convertExpr expr
