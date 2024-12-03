module AST.Base (
    UnOp(..),
    ABinOp(..),
    BBinOp(..),
    RBinOp(..)
) where

-- Unary Operators
data UnOp = Not
          | Neg
          deriving (Show)

-- Arithmetic Binary Operators
data ABinOp = Add
            | Sub
            | Mul
            | Div
            | Mod
            deriving (Show)

-- Boolean Binary Operators
data BBinOp = And
            | Or
            | Xor
            deriving (Show)

-- Relational Binary Operators
data RBinOp = Lt
            | Lte
            | Gt
            | Gte
            | Neq
            | Eq
            deriving (Show)
