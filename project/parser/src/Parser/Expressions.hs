module Parser.Expressions
    ( parseDeclareAssignment
    , parseCondWrapped
    , parseExpr
    ) where

import Control.Applicative
import AST.Expressions
import Parser.Core
import Parser.Types
import Parser.Primitives
import Parser.Operators

parseFuncCall :: String -> Parser Expr
parseFuncCall function = FuncCall function <$> parens (parseExpr `sepBy` ",")

parseCustomFuncCall :: Parser Expr
parseCustomFuncCall = parseCustomFuncCall' ["sqrt", "log", "max", "min", "floor", "ceil", "abs"]
    where
        parseCustomFuncCall' [] = empty
        parseCustomFuncCall' (f:fs) = (parseIdentifier f >>= parseFuncCall) <|> parseCustomFuncCall' fs

parseDeclareAssignment :: Parser (Maybe Expr)
parseDeclareAssignment = optional
    ( parseStr "{" *>
      parseExpr <*
      parseStr "}"
    )

parseDeclare :: Parser Expr
parseDeclare = Declare <$>
    parseType <*>
    ( do
      name <- parseName
      expr <- parseDeclareAssignment
      return (name, expr)
    ) `sepBy` ","

parseAssign :: Parser Expr
parseAssign = Assign <$>
    parseName <*>
    parseAssignOp <*>
    parseExpr

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
parseExpr
    =  parseDeclare
   <|> parseAssign
   <|> expressionParser operatorsTable parseTerm

parseCondWrapped :: String -> Parser Expr
parseCondWrapped s = parseStr s *> parens parseExpr
