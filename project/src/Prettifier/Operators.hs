module Prettifier.Operators
    ( convertAUnOp
    , convertABinOp
    , convertBUnOp
    , convertBBinOp
    , convertRBinOp
    , convertAssignOp
    ) where

import AST.Types.Operators

convertBinary :: String -> String -> String -> String
convertBinary op expr1 expr2 = expr1 ++ " " ++ op ++ " " ++ expr2

convertAssign :: String -> String
convertAssign var = " " ++ var ++ "= "

-- Arithmetic operators
convertAUnOp :: AUnOp -> String -> String
convertAUnOp Neg expr = "-(" ++ expr ++ ")"
convertAUnOp Inc expr = expr ++ "++"
convertAUnOp Dec expr = expr ++ "--"

convertABinOp :: ABinOp -> String -> String -> String
convertABinOp Add = convertBinary "+"
convertABinOp Sub = convertBinary "-"
convertABinOp Mul = convertBinary "*"
convertABinOp Div = convertBinary "/"
convertABinOp Mod = convertBinary "%"
convertABinOp Pow = convertBinary "**"

-- Boolean operators
convertBUnOp :: BUnOp -> String
convertBUnOp Not = "not"

convertBBinOp :: BBinOp -> String
convertBBinOp And = "&&"
convertBBinOp Or  = "||"

-- Relational operators
convertRBinOp :: RBinOp -> String
convertRBinOp Eq  = "=="
convertRBinOp Neq = "!="
convertRBinOp Lt  = "<"
convertRBinOp Lte = "<="
convertRBinOp Gt  = ">"
convertRBinOp Gte = ">="

-- Assignment operators
convertAssignOp :: AssignOp -> String
convertAssignOp Basic = convertAssign ":"
convertAssignOp (With Add) = convertAssign "+"
convertAssignOp (With Sub) = convertAssign "-"
convertAssignOp (With Mul) = convertAssign "*"
convertAssignOp (With Div) = convertAssign "/"
convertAssignOp (With Mod) = convertAssign "%"
convertAssignOp (With Pow) = convertAssign "**"

