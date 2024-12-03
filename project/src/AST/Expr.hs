module AST.Expr (
    AExpr(..),
    BExpr(..)
) where

import AST.Base

-- Arithmetic expressions
data AExpr = Var String
           | IntConst Integer
           | AUnary UnOp AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

-- Boolean expressions
data BExpr = BoolConst Bool
           | BUnary UnOp BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)
