module Prettifier.Statements
    ( prettifyAST
    ) where

import Data.List (isPrefixOf, intercalate)
import Prettifier.Types
import Prettifier.Operators
import Prettifier.Expressions
import AST.Statements

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

addSep :: [String] -> [String]
addSep = map addSemicolon
  where
    addSemicolon stmt
      | any (`isPrefixOf` stmt) ["if", "while", "for", "//"] = stmt
      | otherwise = stmt ++ ";"

convertStmt :: Stmt -> String
convertStmt Break = "break"
convertStmt Continue = "continue"
convertStmt (OneComm comm) = "// " ++ comm
convertStmt (MultiComm comm) = "/* " ++ comm ++ " */"
convertStmt (Expr expr) = convertExpr expr
convertStmt (Read var) = "read(" ++ var ++ ")"
convertStmt (Print expr) = "print(" ++ convertExpr expr ++ ")"
convertStmt (Declare (Decl typ vars)) =
    convertType typ ++ " " ++ intercalate ", " (map convertVar vars)
  where
    convertVar (name, Nothing) = name
    convertVar (name, Just expr) = name ++ "{ " ++ convertExpr expr ++ " }"
convertStmt (Assign var op expr) = var ++ convertAssignOp op ++ convertExpr expr
convertStmt (If cond stmt1 Nothing) =
    "if (" ++ convertExpr cond ++ ") {\n" ++
    indent (convertStmt stmt1) ++
    "}"
convertStmt (If cond stmt1 (Just stmt2)) = case stmt2 of
    Seq [stmt2'] -> "if (" ++ convertExpr cond ++ ") {\n" ++
        indent (convertStmt stmt1) ++
        "}\nelse " ++ convertStmt stmt2'
    _ -> "if (" ++ convertExpr cond ++ ") {\n" ++
         indent (convertStmt stmt1) ++
         "}\nelse {\n" ++
         indent (convertStmt stmt2) ++
         "}"
convertStmt (While cond stmt) =
    "while (" ++ convertExpr cond ++ ") {\n" ++
    indent (convertStmt stmt) ++
    "}"
convertStmt (For initial cond post stmt) =
    "for (" ++ convertStmt initial ++ "; " ++
    convertExpr cond ++ "; " ++
    convertStmt post ++ ") {\n" ++
    indent (convertStmt stmt) ++ "}"
convertStmt (FuncDef retType name params body) =
    convertType retType ++ " " ++ name ++ "(" ++
    intercalate ", " (concatMap convertParam params) ++ ") {\n" ++
    indent (convertStmt body) ++
    "}"
  where
    convertParam (Decl t vars) = map (\(n, _) -> convertType t ++ " " ++ n) vars
convertStmt (Return Nothing) = "return;"
convertStmt (Return (Just expr)) = "return " ++ convertExpr expr ++ ";"
convertStmt (Seq stmts) = unlines $ addSep (map convertStmt stmts)

prettifyAST :: Stmt -> String
prettifyAST = convertStmt
