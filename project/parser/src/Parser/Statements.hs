module Parser.Statements
    ( parseAST
    ) where

import Control.Applicative
import AST.Types.Statements
import Parser.Core
import Parser.Primitives
import Parser.Operators
import Parser.Expressions

parseSkip :: Parser Stmt
parseSkip = parseStr "skip" *> return Skip

parseAssign :: Parser Stmt
parseAssign = do
    var <- parseName
    op <- parseAssignOp
    expr <- parseArithExpr
    return $ Assign op var expr

parsePrint :: Parser Stmt
parsePrint = do
    parseStr "print"
    parseStr "("
    expr <- (String <$> parseText) <|> (AExpr <$> parseArithExpr)
    parseStr ")"
    return $ Print expr

parseRead :: Parser Stmt
parseRead = do
    parseStr "read"
    parseStr "("
    var <- parseName
    parseStr ")"
    return $ Read var

parseElse :: Parser Stmt
parseElse = do
    parseStr "else"
    parseBlockWrapped <|> parseBlock

parseIf :: Parser Stmt
parseIf = do
    cond <- parseCondWrapped "if"
    stmt1 <- parseBlockWrapped
    stmt2 <- parseElse <|> pure Skip
    return $ If cond stmt1 stmt2

parseWhile :: Parser Stmt
parseWhile = do
    cond <- parseCondWrapped "while"
    stmt <- parseBlockWrapped
    return $ While cond stmt

parseFor :: Parser Stmt
parseFor = do
    parseStr "for"
    parseStr "("
    initial <- parseSimpleStmt
    parseStr ";"
    cond <- parseBoolExpr
    parseStr ";"
    update <- parseSimpleStmt
    parseStr ")"
    stmt <- parseBlockWrapped
    return $ For initial cond update stmt

parseSimpleStmt :: Parser Stmt
parseSimpleStmt =  parseAssign
               <|> parsePrint
               <|> parseRead
               <|> parseSkip

parseStmt :: Parser Stmt
parseStmt =  parseIf
         <|> parseWhile
         <|> parseFor
         <|> (parseSimpleStmt <* parseChar ';')

parseBlock :: Parser Stmt
parseBlock = do
    stmts <- some parseStmt
    return $ Seq stmts

parseBlockWrapped :: Parser Stmt
parseBlockWrapped = do
    parseStr "{"
    stmt <- parseBlock
    parseStr "}"
    return stmt

parseAST :: String -> Either ParseError Stmt
parseAST = parseWith (do
    ast <- parseBlock
    parseEOF
    return ast)
