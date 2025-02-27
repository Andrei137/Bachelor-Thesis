module AST.Types.Expressions
    ( AExpr(..)
    , BExpr(..)
    ) where

import AST.Types.Operators

-- Arithmetic expressions
data AExpr
    = Var String
    | IntConst Integer
    | AUnary AUnOp AExpr
    | ABinary ABinOp AExpr AExpr
    | AFuncCall String [AExpr]
    deriving (Show)

-- Boolean expressions
data BExpr
    = BoolConst Bool
    | BUnary BUnOp BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)
