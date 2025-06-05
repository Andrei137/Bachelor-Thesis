module Prettifier.Operators
    ( convertUnaryOp
    , convertBinaryOp
    , convertAssignOp
    ) where

import AST.Operators

convertBinOp :: String -> String -> String -> String
convertBinOp op expr1 expr2 = expr1 ++ " " ++ op ++ " " ++ expr2

convertUnaryOp :: UnaryOp -> String -> String
convertUnaryOp Not expr = "not" ++ expr
convertUnaryOp Neg expr = "-(" ++ expr ++ ")"
convertUnaryOp Inc expr = expr ++ "++"
convertUnaryOp Dec expr = expr ++ "--"

convertBinaryOp :: BinaryOp -> String -> String -> String
convertBinaryOp Add = convertBinOp "+"
convertBinaryOp Sub = convertBinOp "-"
convertBinaryOp Mul = convertBinOp "*"
convertBinaryOp Div = convertBinOp "/"
convertBinaryOp Mod = convertBinOp "%"
convertBinaryOp Pow = convertBinOp "^"
convertBinaryOp And = convertBinOp "&&"
convertBinaryOp Or  = convertBinOp "||"
convertBinaryOp Eq  = convertBinOp "=="
convertBinaryOp Neq = convertBinOp "!="
convertBinaryOp Lt  = convertBinOp "<"
convertBinaryOp Lte = convertBinOp "<="
convertBinaryOp Gt  = convertBinOp ">"
convertBinaryOp Gte = convertBinOp ">="
convertBinaryOp Concat = convertBinOp "."

convertAssign :: String -> String
convertAssign var = " " ++ var ++ "= "

convertAssignOp :: AssignOp -> String
convertAssignOp Basic = convertAssign ":"
convertAssignOp (With Add) = convertAssign "+"
convertAssignOp (With Sub) = convertAssign "-"
convertAssignOp (With Mul) = convertAssign "*"
convertAssignOp (With Div) = convertAssign "/"
convertAssignOp (With Mod) = convertAssign "%"
convertAssignOp (With Pow) = convertAssign "**"
convertAssignOp (With And) = convertAssign "&&"
convertAssignOp (With Or)  = convertAssign "||"
convertAssignOp (With Eq)  = convertAssign "=="
convertAssignOp (With Neq) = convertAssign "!="
convertAssignOp (With Lt)  = convertAssign "<"
convertAssignOp (With Lte) = convertAssign "<="
convertAssignOp (With Gt)  = convertAssign ">"
convertAssignOp (With Gte) = convertAssign ">="
convertAssignOp (With Concat) = convertAssign "."

