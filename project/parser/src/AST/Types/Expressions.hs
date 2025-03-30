{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Types.Expressions
    ( AExpr(..)
    , BExpr(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import AST.Types.Operators

-- Arithmetic expressions
data AExpr
    = Var String
    | IntConst Integer
    | AUnary AUnOp AExpr
    | ABinary ABinOp AExpr AExpr
    | AFuncCall String [AExpr]
    deriving (Show, Eq, Generic)

-- Boolean expressions
data BExpr
    = BoolConst Bool
    | BUnary BUnOp BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show, Eq, Generic)

-- Custom JSON instances
instance ToJSON AExpr where
    toJSON (Var s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("Var" :: String)
        ]
    toJSON (IntConst i) = object
        [ fromString "contents" .= i
        , fromString "tag" .= ("IntConst" :: String)
        ]
    toJSON (AUnary op e) = object
        [ fromString "contents" .= [toJSON op, toJSON e]
        , fromString "tag" .= ("AUnary" :: String)
        ]
    toJSON (ABinary op e1 e2) = object
        [ fromString "contents" .= [toJSON op, toJSON e1, toJSON e2]
        , fromString "tag" .= ("ABinary" :: String)
        ]
    toJSON (AFuncCall name args) = object
        [ fromString "contents" .= (toJSON (fromString name) : map toJSON args)
        , fromString "tag" .= ("AFuncCall" :: String)
        ]

instance ToJSON BExpr where
    toJSON (BoolConst b) = object
        [ fromString "contents" .= b
        , fromString "tag" .= ("BoolConst" :: String)
        ]
    toJSON (BUnary op e) = object
        [ fromString "contents" .= [toJSON op, toJSON e]
        , fromString "tag" .= ("BUnary" :: String)
        ]
    toJSON (BBinary op e1 e2) = object
        [ fromString "contents" .= [toJSON op, toJSON e1, toJSON e2]
        , fromString "tag" .= ("BBinary" :: String)
        ]
    toJSON (RBinary op e1 e2) = object
        [ fromString "contents" .= [toJSON op, toJSON e1, toJSON e2]
        , fromString "tag" .= ("RBinary" :: String)
        ]
