{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Types.Statements
    ( PrintStmt(..)
    , Stmt(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import AST.Types.Operators
import AST.Types.Expressions

-- Print statements
data PrintStmt
    = String String
    | AExpr AExpr
    deriving (Show, Eq, Generic)

-- Statements
data Stmt
    = Skip
    | Read String
    | Print PrintStmt
    | Assign AssignOp String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | For Stmt BExpr Stmt Stmt
    | Seq [Stmt]
    deriving (Show, Eq, Generic)

-- Custom JSON instances
instance ToJSON PrintStmt where
    toJSON (String s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("String" :: String)
        ]
    toJSON (AExpr e) = object
        [ fromString "contents" .= toJSON e
        , fromString "tag" .= ("AExpr" :: String)
        ]

instance ToJSON Stmt where
    toJSON Skip = object
        [fromString "tag" .= ("Skip" :: String)
        ]
    toJSON (Read s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("Read" :: String)
        ]
    toJSON (Print p) = object
        [ fromString "contents" .= toJSON p
        , fromString "tag" .= ("Print" :: String)
        ]
    toJSON (Assign op var expr) = object
        [ fromString "contents" .= [toJSON op, toJSON var, toJSON expr]
        , fromString "tag" .= ("Assign" :: String)
        ]
    toJSON (If cond thenStmt elseStmt) = object
        [ fromString "contents" .= [toJSON cond, toJSON thenStmt, toJSON elseStmt]
        , fromString "tag" .= ("If" :: String)
        ]
    toJSON (While cond body) = object
        [ fromString "contents" .= [toJSON cond, toJSON body]
        , fromString "tag" .= ("While" :: String)
        ]
    toJSON (For init' cond step body) = object
        [ fromString "contents" .= [toJSON init', toJSON cond, toJSON step, toJSON body]
        , fromString "tag" .= ("For" :: String)
        ]
    toJSON (Seq stmts) = object
        [ fromString "contents" .= map toJSON stmts
        , fromString "tag" .= ("Seq" :: String)
        ]
