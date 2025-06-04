{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Statements
    ( Stmt(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Key (fromString)
import AST.Types
import AST.Expressions

data Stmt
    = Break
    | Continue
    | OneComm String
    | MultiComm String
    | Expr Expr
    | Read String
    | Print Expr
    | If Expr Stmt (Maybe Stmt)
    | While Expr Stmt
    | For Expr Expr Expr Stmt
    | FuncDef Type String [(Type, String, Maybe Expr)] Stmt
    | Return (Maybe Expr)
    | Seq [Stmt]
    deriving (Show, Eq, Generic)

instance ToJSON Stmt where
    toJSON Break = object
        [fromString "tag" .= ("Break" :: String)
        ]
    toJSON Continue = object
        [fromString "tag" .= ("Continue" :: String)
        ]
    toJSON (OneComm comm) = object
        [ fromString "contents" .= comm
        , fromString "tag" .= ("OneComm" :: String)
        ]
    toJSON (MultiComm comm) = object
        [ fromString "contents" .= comm
        , fromString "tag" .= ("MultiComm" :: String)
        ]
    toJSON (Expr e) = object
        [ fromString "contents" .= toJSON e
        , fromString "tag" .= ("Expr" :: String)
        ]
    toJSON (Read s) = object
        [ fromString "contents" .= s
        , fromString "tag" .= ("Read" :: String)
        ]
    toJSON (Print p) = object
        [ fromString "contents" .= toJSON p
        , fromString "tag" .= ("Print" :: String)
        ]
    toJSON (If cond thenStmt Nothing) = object
        [ fromString "contents" .= [toJSON cond, toJSON thenStmt]
        , fromString "tag" .= ("If" :: String)
        ]
    toJSON (If cond thenStmt (Just elseStmt)) = object
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
    toJSON (FuncDef name args typ body) = object
        [ fromString "contents" .= [ toJSON name, toJSON args, toJSON typ, toJSON body ]
        , fromString "tag" .= ("FuncDef" :: String)
        ]
    toJSON (Return Nothing) = object
        [ fromString "tag" .= ("Return" :: String)
        ]
    toJSON (Return (Just expr)) = object
        [ fromString "contents" .= toJSON expr
        , fromString "tag" .= ("Return" :: String)
        ]
    toJSON (Seq stmts) = object
        [ fromString "contents" .= map toJSON stmts
        , fromString "tag" .= ("Seq" :: String)
        ]
