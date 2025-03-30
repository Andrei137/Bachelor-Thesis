module Prettifier.Expressions
    ( convertAExpr
    , convertBExpr
    ) where

import Prettifier.Operators
import AST.Types.Expressions

convertAExpr :: AExpr -> String
-- Var String
convertAExpr (Var var) = var
-- IntConst Integer
convertAExpr (IntConst i) = show i
-- AUnary AUnOp AExpr
convertAExpr (AUnary op expr) = convertAUnOp op (convertAExpr expr)
-- ABinary ABinOp AExpr AExpr
convertAExpr (ABinary op expr1 expr2) = convertABinOp op (convertAExpr expr1) (convertAExpr expr2)
-- AFuncCall String [AExpr]
convertAExpr (AFuncCall func args) = func ++ "(" ++ unwords (map convertAExpr args) ++ ")"

convertBExpr :: BExpr -> String
-- BoolConst Bool
convertBExpr (BoolConst b) = show b
-- BUnary BUnOp BExpr
convertBExpr (BUnary op expr) =
    convertBUnOp op ++ " " ++
    convertBExpr expr
-- BBinary BBinOp BExpr BExpr
convertBExpr (BBinary op expr1 expr2) =
    let expr1' = convertBExpr expr1
        expr2' = convertBExpr expr2
        op' = convertBBinOp op
    in case (expr1, expr2) of
        (BoolConst _, BoolConst _) -> expr1' ++ " " ++ op' ++ " " ++ expr2'
        _ -> "(" ++ expr1' ++ ") " ++ op' ++ " (" ++ expr2' ++ ")"
-- RBinary RBinOp AExpr AExpr
convertBExpr (RBinary op expr1 expr2) =
    convertAExpr expr1 ++
    " " ++ convertRBinOp op ++ " " ++
    convertAExpr expr2
