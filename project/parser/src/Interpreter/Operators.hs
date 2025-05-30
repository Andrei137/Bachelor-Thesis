module Interpreter.Operators
    ( interpretAUnOp
    , interpretABinOp
    , interpretBUnOp
    , interpretBBinOp
    , interpretRBinOp
    , isRelational
    ) where

import AST.Operators

interpretAUnOp :: UnaryOp -> (Integer -> Integer)
interpretAUnOp Neg = negate
interpretAUnOp Inc = (+ 1)
interpretAUnOp Dec = subtract 1
interpretAUnOp _ = error "Unary operator not supported"

interpretABinOp :: BinaryOp -> (Integer -> Integer -> Integer)
interpretABinOp Add = (+)
interpretABinOp Sub = (-)
interpretABinOp Mul = (*)
interpretABinOp Div = div
interpretABinOp Mod = mod
interpretABinOp Pow = (^)
interpretABinOp _ = error "Binary operator not supported"

interpretBUnOp :: UnaryOp -> (Bool -> Bool)
interpretBUnOp Not = not
interpretBUnOp _ = error "Unary boolean operator not supported"

interpretBBinOp :: BinaryOp -> (Bool -> Bool -> Bool)
interpretBBinOp And = (&&)
interpretBBinOp Or = (||)
interpretBBinOp _ = error "Binary boolean operator not supported"

interpretRBinOp :: BinaryOp -> (Integer -> Integer -> Bool)
interpretRBinOp Eq  = (==)
interpretRBinOp Neq = (/=)
interpretRBinOp Lt  = (<)
interpretRBinOp Lte = (<=)
interpretRBinOp Gt  = (>)
interpretRBinOp Gte = (>=)
interpretRBinOp _ = error "Relational operator not supported"

isRelational :: BinaryOp -> Bool
isRelational Eq  = True
isRelational Neq = True
isRelational Lt  = True
isRelational Lte = True
isRelational Gt  = True
isRelational Gte = True
isRelational _ = False
