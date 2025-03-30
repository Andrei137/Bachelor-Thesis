module AST.Formatter.Statements
    ( formatAST
    ) where

import Control.Monad.Reader (Reader, local)
import AST.Types.Statements
import AST.Formatter.Core
import AST.Formatter.Expressions

formatStmt :: Stmt -> Reader Int String
-- Skip
formatStmt Skip = do
    i <- indent
    return $ i ++ "Skip"
-- Read String
formatStmt (Read var) = do
    i <- indent
    return $ i ++ "Read \"" ++ var ++ "\""
-- Print AExpr
formatStmt (Print expr) = do
    i <- indent
    case expr of
        String str -> return $ i ++ "Print \"" ++ str ++ "\""
        AExpr expr' -> do
            exprStr <- local (+1) (formatAExpr expr')
            return $ i ++ "Print " ++ exprStr
-- Assign String AExpr
formatStmt (Assign op var expr) = do
    i <- indent
    exprStr <- local (+1) (formatAExpr expr)
    return $ i ++ "Assign " ++ show op ++ " \"" ++ var ++ "\" " ++ exprStr
-- If BExpr Stmt Stmt
formatStmt (If cond stmt1 stmt2) = do
    i <- indent
    stmt1Str <- local (+1) $ formatStmt stmt1
    stmt2Str <- local (+1) $ formatStmt stmt2
    return $ i ++ "If " ++ show cond ++ "\n" ++
             stmt1Str ++ "\n" ++ i ++ "Else\n" ++
             stmt2Str
-- While BExpr Stmt
formatStmt (While cond stmt') = do
    i <- indent
    stmtStr <- local (+1) $ formatStmt stmt'
    return $ i ++ "While " ++ show cond ++ "\n" ++ stmtStr
-- For Stmt BExpr Stmt Stmt
formatStmt (For initial cond update stmt') = do
    i <- indent
    initStr <- local (+1) $ formatStmt initial
    updateStr <- local (+1) $ formatStmt update
    stmtStr <- local (+1) $ formatStmt stmt'
    return $ i ++ "For\n" ++
             initStr ++ "\n" ++ i ++ "\t" ++ show cond ++ "\n" ++
             updateStr ++ "\n" ++ stmtStr
-- Seq [Stmt]
formatStmt (Seq stmts) = do
    i <- indent
    stmtsStr <- mapM (\stmt' -> local (+1) (formatStmt stmt')) stmts
    return $ i ++ "Seq [" ++
             concatMap (\stmt' -> "\n" ++ stmt') stmtsStr ++
             "\n" ++ i ++ "]"

formatAST :: Stmt -> String
formatAST = formatWith formatStmt
