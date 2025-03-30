module Parser.Expressions
    ( parseArithExpr
    , parseBoolExpr
    , parseCondWrapped
    ) where

import Control.Applicative
import AST.Types.Expressions
import Parser.Core
import Parser.Primitives
import Parser.Operators

-- Arihtmetic
sepBy :: Parser a -> String -> Parser [a]
sepBy p sep = do
    first <- p
    rest <- many (do
                    parseStr sep
                    next <- p
                    return next
                )
    return (first : rest)

parseArgs :: Parser [AExpr]
parseArgs = parseArithExpr `sepBy` ","

parseFuncCall :: String -> Parser AExpr
parseFuncCall function = do
    parseStr function
    parseStr "("
    args <- parseArgs
    parseStr ")"
    return $ AFuncCall function args

parseArithTerm :: Parser AExpr
parseArithTerm
    =  parens parseArithExpr
   <|> parseFuncCall "sqrt"
   <|> IntConst <$> parseNumber
   <|> Var <$> parseName

parseArithExpr :: Parser AExpr
parseArithExpr = expressionParser arithOperatorsTable parseArithTerm

-- Boolean
parseBoolTerm :: Parser BExpr
parseBoolTerm
    =  parens parseBoolExpr
   <|> BoolConst <$> parseBool
   <|> parseRelExpr

parseBoolExpr :: Parser BExpr
parseBoolExpr = expressionParser boolOperatorsTable parseBoolTerm

parseCondWrapped :: String -> Parser BExpr
parseCondWrapped s = do
    parseStr s
    parseStr "("
    cond <- parseBoolExpr
    parseStr ")"
    return cond

-- Relational
parseRelExpr :: Parser BExpr
parseRelExpr = do
    a1 <- parseArithExpr
    op <- parseRelOp
    a2 <- parseArithExpr
    return $ RBinary op a1 a2
