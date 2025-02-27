module Prettifier.Statements
    ( prettifyAST
    ) where

import Data.List (isPrefixOf)
import Prettifier.Operators
import Prettifier.Expressions
import AST.Types.Statements

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

addSep :: [String] -> [String]
addSep = map addSemicolon
  where
    addSemicolon stmt
      | any (`isPrefixOf` stmt) ["if", "while", "for"] = stmt
      | otherwise = stmt ++ ";"

convertStmt :: Stmt -> String
-- Skip
convertStmt Skip = "skip"
-- Read String
convertStmt (Read var) = "read(" ++ var ++ ")"
-- Print AExpr
convertStmt (Print expr) = case expr of
    String str -> "print(\"" ++ str ++ "\")"
    AExpr expr' -> "print(" ++ convertAExpr expr' ++ ")"
-- Assign String AExpr
convertStmt (Assign op var expr) = var ++ convertAssignOp op ++ convertAExpr expr
-- If BExpr Stmt Stmt
convertStmt (If cond stmt1 stmt2) = case stmt2 of
    Skip -> "if (" ++ convertBExpr cond ++ ") {\n" ++
        indent (convertStmt stmt1) ++
        "}"
    Seq [stmt2'] -> "if (" ++ convertBExpr cond ++ ") {\n" ++
        indent (convertStmt stmt1) ++
        "}\nelse " ++ convertStmt stmt2'
    _ -> "if (" ++ convertBExpr cond ++ ") {\n" ++
         indent (convertStmt stmt1) ++
         "}\nelse {\n" ++
         indent (convertStmt stmt2) ++
         "}"
-- While BExpr Stmt
convertStmt (While cond stmt) =
    "while (" ++ convertBExpr cond ++ ") {\n" ++
    indent (convertStmt stmt) ++
    "}"
-- For Stmt BExpr Stmt Stmt
convertStmt (For initial cond post stmt) =
    "for (" ++ convertStmt initial ++ "; " ++
    convertBExpr cond ++ "; " ++
    convertStmt post ++ ") {\n" ++
    indent (convertStmt stmt) ++ "}"
-- Seq [Stmt]
convertStmt (Seq stmts) = unlines $ addSep (map convertStmt stmts)

prettifyAST :: Stmt -> String
prettifyAST = convertStmt
