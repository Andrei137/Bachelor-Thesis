module Interpreter.Operators
    ( interpretAUnOp
    , interpretABinOp
    , interpretBUnOp
    , interpretBBinOp
    , interpretRBinOp
    ) where

import AST.Types.Operators

-- Arithmetic operators
interpretAUnOp :: AUnOp -> (Integer -> Integer)
interpretAUnOp Neg = negate
interpretAUnOp Inc = (+ 1)
interpretAUnOp Dec = subtract 1

interpretABinOp :: ABinOp -> (Integer -> Integer -> Integer)
interpretABinOp Add = (+)
interpretABinOp Sub = (-)
interpretABinOp Mul = (*)
interpretABinOp Div = div
interpretABinOp Mod = mod
interpretABinOp Pow = (^)

-- Boolean operators
interpretBUnOp :: BUnOp -> (Bool -> Bool)
interpretBUnOp Not = not

interpretBBinOp :: BBinOp -> (Bool -> Bool -> Bool)
interpretBBinOp And = (&&)
interpretBBinOp Or = (||)

-- Relational operators
interpretRBinOp :: RBinOp -> (Integer -> Integer -> Bool)
interpretRBinOp Eq  = (==)
interpretRBinOp Neq = (/=)
interpretRBinOp Lt  = (<)
interpretRBinOp Lte = (<=)
interpretRBinOp Gt  = (>)
interpretRBinOp Gte = (>=)
