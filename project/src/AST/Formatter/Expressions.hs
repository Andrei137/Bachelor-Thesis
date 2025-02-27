module AST.Formatter.Expressions
    ( formatAExpr
    ) where

import Control.Monad.Reader (Reader, local)
import AST.Types.Expressions
import AST.Formatter.Core (indent)

formatAExpr :: AExpr -> Reader Int String
-- Var String
formatAExpr (Var name) = do
    return $ "Var \"" ++ name ++ "\""
-- IntConst Integer
formatAExpr (IntConst n) = do
    return $ "IntConst " ++ show n
-- AUnary AUnOp AExpr
formatAExpr (AUnary op expr) = do
    exprStr <- local (+1) (formatAExpr expr)
    return $ "AUnary " ++ show op ++
             " (" ++ exprStr ++ ")"
-- ABinary ABinOp AExpr AExpr
formatAExpr (ABinary op left right) = do
    i <- indent
    leftStr <- local (+1) (formatAExpr left)
    rightStr <- local (+1) (formatAExpr right)
    return $ "ABinary " ++ show op ++ "\n" ++
             i ++ leftStr ++ "\n" ++
             i ++ rightStr
-- AFuncCall String [AExpr]
formatAExpr (AFuncCall name args) = do
    i <- indent
    argsStr <- local (+1) (mapM formatAExpr args)
    return $ "AFuncCall \"" ++ name ++ "\" [\n" ++
             i ++ unlines argsStr ++ "]"
