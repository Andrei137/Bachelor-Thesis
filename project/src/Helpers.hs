module Helpers (
    prettify,
    reconstruct
) where

import AST.Base
import AST.Expr
import AST.Stmt
import Control.Monad.Reader

prettify :: Stmt -> String
prettify stmt = runReader (prettify' stmt) 0
    where
    indent = do
        level <- ask
        return $ replicate (level * 4) ' '

    prettify' (Seq stmts) = do
        i <- indent
        stmtsStr <- mapM (\stmt' -> local (+1) (prettify' stmt')) stmts
        return $ i ++ "Seq [" ++ concatMap (\stmt' -> "\n" ++ stmt') stmtsStr ++ "\n" ++ i ++ "]"

    prettify' (Assign var expr) = do
        i <- indent
        return $ i ++ "Assign \"" ++ var ++ "\" (" ++ show expr ++ ")"

    prettify' (If cond stmt1 stmt2) = do
        i <- indent
        stmt1Str <- local (+1) $ prettify' stmt1
        stmt2Str <- local (+1) $ prettify' stmt2
        return $ i ++ "If " ++ show cond ++ "\n" ++
                 stmt1Str ++ "\n" ++ i ++ "Else\n" ++
                 stmt2Str

    prettify' (While cond stmt') = do
        i <- indent
        stmtStr <- local (+1) $ prettify' stmt'
        return $ i ++ "While " ++ show cond ++ "\n" ++ stmtStr

    prettify' Skip = do
        i <- indent
        return $ i ++ "Skip"

reconstruct :: Stmt -> String
reconstruct = reconstruct'
    where
    indent = unlines . map ("\t" ++) . lines

    addSep [] = []
    addSep [lastStmt] = [lastStmt]
    addSep (stmt:stmts) = (stmt ++ ";"):(addSep stmts)

    reconstructOpA Add = "+"
    reconstructOpA Sub = "-"
    reconstructOpA Mul = "*"
    reconstructOpA Div = "/"
    reconstructOpA Mod = "%"

    reconstructOpB And = "and"
    reconstructOpB Or  = "or"
    reconstructOpB Xor = "xor"

    reconstructOpR Lt  = "<"
    reconstructOpR Lte = "<="
    reconstructOpR Gt  = ">"
    reconstructOpR Gte = ">="
    reconstructOpR Neq = "!="
    reconstructOpR Eq  = "=="

    reconstructA (Var var) = var
    reconstructA (IntConst n) = show n
    reconstructA (AUnary _ expr) = "-" ++ reconstructA expr
    reconstructA (ABinary op expr1 expr2) =
        reconstructA expr1 ++
        " " ++ reconstructOpA op ++ " " ++
        reconstructA expr2

    reconstructB (BoolConst b) = show b
    reconstructB (BUnary _ expr) = "not " ++ reconstructB expr
    reconstructB (BBinary op expr1 expr2) =
        "(" ++ reconstructB expr1 ++
        " " ++ reconstructOpB op ++ " " ++
        reconstructB expr2 ++ ")"
    reconstructB (RBinary op expr1 expr2)  =
        "(" ++ reconstructA expr1 ++
        " " ++ reconstructOpR op ++ " " ++
        reconstructA expr2 ++ ")"

    reconstruct' Skip = "skip"
    reconstruct' (Assign var expr) = var ++ " := " ++ reconstructA expr
    reconstruct' (If cond stmt1 stmt2) =
        "if " ++ reconstructB cond ++ " then\n" ++
        indent (reconstruct stmt1) ++
        "else\n" ++
        indent (reconstruct stmt2) ++
        "end"
    reconstruct' (While cond stmt) =
        "while " ++ reconstructB cond ++ " do\n" ++
        indent (reconstruct stmt) ++
        "end"
    reconstruct' (Seq stmts) = unlines $ addSep (map reconstruct' stmts)
