{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Operators
    ( UnaryOp(..)
    , BinaryOp(..)
    , AssignOp(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))

data UnaryOp
    = Neg
    | Inc
    | Dec
    | Not
    deriving (Show, Eq, Generic)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | Concat
    deriving (Show, Eq, Generic)

data AssignOp
    = Basic
    | With BinaryOp
    deriving (Show, Eq, Generic)

instance ToJSON UnaryOp
instance ToJSON BinaryOp
instance ToJSON AssignOp where
    toJSON Basic = "Basic"
    toJSON (With op) = toJSON op
