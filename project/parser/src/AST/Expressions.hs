{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Expressions
    ( Expr(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import AST.Types
import AST.Operators

data Expr
    = Var String
    | FuncCall String [Expr]
    | IntConst Integer
    | DoubleConst Double
    | BoolConst Bool
    | CharConst Char
    | StringConst String
    | UnaryOp UnaryOp Expr
    | BinaryOp BinaryOp Expr Expr
    | Declare Type [(String, Maybe Expr)]
    | Assign String AssignOp Expr
    deriving (Show, Eq, Generic)

instance ToJSON Expr where
    toJSON (Var s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("Var" :: String)
        ]
    toJSON (FuncCall name args) = object
        [ fromString "contents" .= (toJSON (fromString name) : map toJSON args)
        , fromString "tag" .= ("FuncCall" :: String)
        ]
    toJSON (IntConst i) = object
        [ fromString "contents" .= i
        , fromString "tag" .= ("IntConst" :: String)
        ]
    toJSON (DoubleConst d) = object
        [ fromString "contents" .= d
        , fromString "tag" .= ("DoubleConst" :: String)
        ]
    toJSON (BoolConst b) = object
        [ fromString "contents" .= b
        , fromString "tag" .= ("BoolConst" :: String)
        ]
    toJSON (CharConst c) = object
        [ fromString "contents" .= c
        , fromString "tag" .= ("CharConst" :: String)
        ]
    toJSON (StringConst s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("StringConst" :: String)
        ]
    toJSON (UnaryOp op e) = object
        [ fromString "contents" .= [toJSON op, toJSON e]
        , fromString "tag" .= ("UnaryOp" :: String)
        ]
    toJSON (BinaryOp op e1 e2) = object
        [ fromString "contents" .= [toJSON op, toJSON e1, toJSON e2]
        , fromString "tag" .= ("BinaryOp" :: String)
        ]
    toJSON (Declare typ []) = object
        [ fromString "contents" .= [toJSON typ]
        , fromString "tag" .= ("Declare" :: String)
        ]
    toJSON (Declare name [(typ, Nothing)]) = object
        [ fromString "contents" .= [toJSON name, toJSON typ]
        , fromString "tag" .= ("Declare" :: String)
        ]
    toJSON (Declare name [(typ, Just expr)]) = object
        [ fromString "contents" .= [toJSON name, toJSON typ, toJSON expr]
        , fromString "tag" .= ("Declare" :: String)
        ]
    toJSON (Declare name (x:xs)) = object
        [ fromString "contents" .= (toJSON name : toJSON x : map toJSON xs)
        , fromString "tag" .= ("Declare" :: String)
        ]
    toJSON (Assign op var expr) = object
        [ fromString "contents" .= [toJSON op, toJSON var, toJSON expr]
        , fromString "tag" .= ("Assign" :: String)
        ]
