module AST.Types.Operators
    ( AUnOp(..)
    , BUnOp(..)
    , ABinOp(..)
    , BBinOp(..)
    , RBinOp(..)
    , AssignOp(..)
    ) where

-- Arithmetic operators
data AUnOp
    = Neg
    | Inc
    | Dec
    deriving (Show)

data ABinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    deriving (Show)

-- Boolean operators
data BUnOp =
    Not
    deriving (Show)

data BBinOp
    = And
    | Or
    deriving (Show)

-- Relational operators
data RBinOp
    = Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    deriving (Show)

-- Assignment operators
data AssignOp
    = Basic
    | With ABinOp
    deriving (Show)
