module Parser.Expressions
    ( parseCondWrapped
    , parseExpr
    ) where

import Control.Applicative
import AST.Expressions
import Parser.Core
import Parser.Primitives
import Parser.Operators

parseFuncCall :: String -> Parser Expr
parseFuncCall function = FuncCall function <$> parens (parseExpr `sepBy` ",")

parseCustomFuncCall :: Parser Expr
parseCustomFuncCall = parseCustomFuncCall' ["sqrt", "log", "max", "min", "floor", "ceil", "abs"]
    where
        parseCustomFuncCall' [] = empty
        parseCustomFuncCall' (f:fs) = (parseIdentifier f >>= parseFuncCall) <|> parseCustomFuncCall' fs

parseTerm :: Parser Expr
parseTerm
    =  parens parseExpr
   <|> (parseName >>= parseFuncCall)
   <|> parseCustomFuncCall
   <|> DoubleConst <$> parseDouble
   <|> IntConst <$> parseInteger
   <|> BoolConst <$> parseBool
   <|> CharConst <$> parseAnyChar
   <|> StringConst <$> parseAnyText
   <|> Var <$> parseName

parseExpr :: Parser Expr
parseExpr = expressionParser operatorsTable parseTerm

parseCondWrapped :: String -> Parser Expr
parseCondWrapped s = do
    parseStr s
    parens parseExpr
