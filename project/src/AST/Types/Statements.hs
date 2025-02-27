module AST.Types.Statements
    ( PrintStmt(..)
    , Stmt(..)
    ) where

import AST.Types.Operators
import AST.Types.Expressions

-- Statements
data PrintStmt
    = String String
    | AExpr AExpr
    deriving (Show)

data Stmt
    = Skip
    | Read String
    | Print PrintStmt
    | Assign AssignOp String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | For Stmt BExpr Stmt Stmt
    | Seq [Stmt]
    deriving (Show)
