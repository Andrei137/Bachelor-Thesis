module AST.Stmt (
    Stmt(..)
) where

import AST.Expr

-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)
