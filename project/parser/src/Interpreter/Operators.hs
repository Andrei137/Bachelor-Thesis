{-# LANGUAGE LambdaCase #-}
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
interpretAUnOp = \case
  Neg -> negate
  Inc -> (+1)
  Dec -> subtract 1
  _   -> error "Unary arithmetic operator not supported"

interpretABinOp :: BinaryOp -> (Integer -> Integer -> Integer)
interpretABinOp = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div
  Mod -> mod
  Pow -> (^)
  _   -> error "Binary arithmetic operator not supported"

interpretBUnOp :: UnaryOp -> (Bool -> Bool)
interpretBUnOp = \case
    Not -> not
    _ -> error "Unary boolean operator Inc not supported"

interpretBBinOp :: BinaryOp -> (Bool -> Bool -> Bool)
interpretBBinOp = \case
  And -> (&&)
  Or  -> (||)
  _   -> error "Binary boolean operator not supported"

interpretRBinOp :: BinaryOp -> (Integer -> Integer -> Bool)
interpretRBinOp = \case
  Eq  -> (==)
  Neq -> (/=)
  Lt  -> (<)
  Lte -> (<=)
  Gt  -> (>)
  Gte -> (>=)
  _   -> error "Relational operator not supported"

isRelational :: BinaryOp -> Bool
isRelational = \case
  Eq  -> True
  Neq -> True
  Lt  -> True
  Lte -> True
  Gt  -> True
  Gte -> True
  _ -> False

