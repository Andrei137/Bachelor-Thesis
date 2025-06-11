{-# LANGUAGE LambdaCase #-}
module Interpreter.Operators
    ( interpretUnaryOp
    , interpretBinaryOp
    ) where

import Interpreter.Core
import AST.Operators

errorStr :: String -> String -> String
errorStr op typ = op ++ " operator only supported for " ++ typ ++ " types"

interpretUnaryOp :: UnaryOp -> Value -> Value
interpretUnaryOp Neg = \case
  IntVal i    -> IntVal (negate i)
  DoubleVal d -> DoubleVal (negate d)
  _           -> error $ errorStr "Negation" "numeric"
interpretUnaryOp Inc = \case
  IntVal i    -> IntVal (i + 1)
  DoubleVal d -> DoubleVal (d + 1)
  _           -> error $ errorStr "Increment" "numeric"
interpretUnaryOp Dec = \case
  IntVal i    -> IntVal (i - 1)
  DoubleVal d -> DoubleVal (d - 1)
  _           -> error $ errorStr "Decrement" "numeric"
interpretUnaryOp Not = \case
  BoolVal b -> BoolVal (not b)
  _         -> error $ errorStr "Logical negation" "boolean"

interpretBinaryOp :: BinaryOp -> Value -> Value -> Value
interpretBinaryOp Add = \case
  IntVal i1 -> \case
    IntVal i2    -> IntVal (i1 + i2)
    DoubleVal d2 -> DoubleVal (fromIntegral i1 + d2)
    _            -> error $ errorStr "Addition" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> DoubleVal (d1 + fromIntegral i2)
    DoubleVal d2 -> DoubleVal (d1 + d2)
    _            -> error $ errorStr "Addition" "numeric"
  _ -> error $ errorStr "Addition" "numeric"
interpretBinaryOp Sub = \case
  IntVal i1 -> \case
    IntVal i2    -> IntVal (i1 - i2)
    DoubleVal d2 -> DoubleVal (fromIntegral i1 - d2)
    _            -> error $ errorStr "Subtraction" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> DoubleVal (d1 - fromIntegral i2)
    DoubleVal d2 -> DoubleVal (d1 - d2)
    _            -> error $ errorStr "Subtraction" "numeric"
  _ -> error $ errorStr "Subtraction" "numeric"
interpretBinaryOp Mul = \case
  IntVal i1 -> \case
    IntVal i2    -> IntVal (i1 * i2)
    DoubleVal d2 -> DoubleVal (fromIntegral i1 * d2)
    _            -> error $ errorStr "Multiplication" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> DoubleVal (d1 * fromIntegral i2)
    DoubleVal d2 -> DoubleVal (d1 * d2)
    _            -> error $ errorStr "Multiplication" "numeric"
  _ -> error $ errorStr "Multiplication" "numeric"
interpretBinaryOp Div = \case
  IntVal i1 -> \case
    IntVal i2    -> if i2 == 0 then error "Division by zero" else IntVal (i1 `div` i2)
    DoubleVal d2 -> if d2 == 0 then error "Division by zero" else DoubleVal (fromIntegral i1 / d2)
    _            -> error $ errorStr "Division" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> if i2 == 0 then error "Division by zero" else DoubleVal (d1 / fromIntegral i2)
    DoubleVal d2 -> if d2 == 0 then error "Division by zero" else DoubleVal (d1 / d2)
    _            -> error $ errorStr "Division" "numeric"
  _ -> error $ errorStr "Division" "numeric"
interpretBinaryOp Mod = \case
  IntVal i1 -> \case
    IntVal i2 -> if i2 == 0 then error "Division by zero" else IntVal (i1 `mod` i2)
    _         -> error $ errorStr "Modulo" "integer"
  _ -> error $ errorStr "Modulo" "integer"
interpretBinaryOp Pow = \case
  IntVal i1 -> \case
    IntVal i2    -> IntVal (i1 ^ i2)
    DoubleVal d2 -> DoubleVal (fromIntegral i1 ** d2)
    _            -> error $ errorStr "Power" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> DoubleVal (d1 ** fromIntegral i2)
    DoubleVal d2 -> DoubleVal (d1 ** d2)
    _            -> error $ errorStr "Power" "numeric"
  _ -> error $ errorStr "Power" "numeric"
interpretBinaryOp And = \case
  BoolVal b1 -> \case
    BoolVal b2 -> BoolVal (b1 && b2)
    _          -> error $ errorStr "Logical AND" "boolean"
  _ -> error $ errorStr "Logical AND" "boolean"
interpretBinaryOp Or = \case
  BoolVal b1 -> \case
    BoolVal b2 -> BoolVal (b1 || b2)
    _          -> error $ errorStr "Logical OR" "boolean"
  _ -> error $ errorStr "Logical OR" "boolean"
interpretBinaryOp Eq = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 == i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 == d2)
    _            -> error "Illegal equality check"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 == fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 == d2)
    _            -> error "Illegal equality check"
  BoolVal b1 -> \case
    BoolVal b2 -> BoolVal (b1 == b2)
    _          -> error "Illegal equality check"
  CharVal c1 -> \case
    CharVal c2 -> BoolVal (c1 == c2)
    _          -> error "Illegal equality check"
  StringVal s1 -> \case
    StringVal s2 -> BoolVal (s1 == s2)
    _            -> error "Illegal equality check"
  _ -> error "Illegal equality check"
interpretBinaryOp Neq = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 /= i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 /= d2)
    _            -> error "Illegal inequality check"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 /= fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 /= d2)
    _            -> error "Illegal inequality check"
  BoolVal b1 -> \case
    BoolVal b2 -> BoolVal (b1 /= b2)
    _          -> error "Illegal inequality check"
  CharVal c1 -> \case
    CharVal c2 -> BoolVal (c1 /= c2)
    _          -> error "Illegal inequality check"
  StringVal s1 -> \case
    StringVal s2 -> BoolVal (s1 /= s2)
    _            -> error "Illegal inequality check"
  _ -> error "Illegal inequality check"
interpretBinaryOp Lt = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 < i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 < d2)
    _            -> error $ errorStr "Less than" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 < fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 < d2)
    _            -> error $ errorStr "Less than" "numeric"
  _ -> error $ errorStr "Less than" "numeric"
interpretBinaryOp Lte = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 <= i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 <= d2)
    _            -> error $ errorStr "Less than or equal" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 <= fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 <= d2)
    _            -> error $ errorStr "Less than or equal" "numeric"
  _ -> error $ errorStr "Less than or equal" "numeric"
interpretBinaryOp Gt = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 > i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 > d2)
    _            -> error $ errorStr "Greater than" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 > fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 > d2)
    _            -> error $ errorStr "Greater than" "numeric"
  _ -> error $ errorStr "Greater than" "numeric"
interpretBinaryOp Gte = \case
  IntVal i1 -> \case
    IntVal i2    -> BoolVal (i1 >= i2)
    DoubleVal d2 -> BoolVal (fromIntegral i1 >= d2)
    _            -> error $ errorStr "Greater than or equal" "numeric"
  DoubleVal d1 -> \case
    IntVal i2    -> BoolVal (d1 >= fromIntegral i2)
    DoubleVal d2 -> BoolVal (d1 >= d2)
    _            -> error $ errorStr "Greater than or equal" "numeric"
  _ -> error $ errorStr "Greater than or equal" "numeric"
interpretBinaryOp Concat = \case
  StringVal s1 -> \case
    StringVal s2 -> StringVal (s1 ++ s2)
    _            -> error $ errorStr "Concatenation" "string"
  _ -> error $ errorStr "Concatenation" "string"
