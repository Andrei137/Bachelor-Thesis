{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Types.Operators
    ( AUnOp(..)
    , ABinOp(..)
    , BUnOp(..)
    , BBinOp(..)
    , RBinOp(..)
    , AssignOp(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))

-- Arithmetic operators
data AUnOp
    = Neg
    | Inc
    | Dec
    deriving (Show, Eq, Generic)

data ABinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    deriving (Show, Eq, Generic)

-- Boolean operators
data BUnOp
    = Not
    deriving (Show, Eq, Generic)

data BBinOp
    = And
    | Or
    deriving (Show, Eq, Generic)

-- Relational operators
data RBinOp
    = Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    deriving (Show, Eq, Generic)

-- Assignment operators
data AssignOp
    = Basic
    | With ABinOp
    deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON AUnOp
instance ToJSON ABinOp
instance ToJSON BUnOp
instance ToJSON BBinOp
instance ToJSON RBinOp

instance ToJSON AssignOp where
    toJSON Basic = "Basic"
    toJSON (With op) = toJSON op
