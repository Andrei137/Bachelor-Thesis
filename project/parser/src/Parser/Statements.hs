module Parser.Statements
    ( parseAST
    ) where

import Control.Applicative
import AST.Types
import AST.Expressions
import AST.Statements
import Parser.Core
import Parser.Types
import Parser.Primitives
import Parser.Expressions

parseComment :: Parser Stmt
parseComment =
    parseStr "//" *>
    ( OneComm <$>
      parseTextUntil "\n\r"
    )

parseMultiComment :: Parser Stmt
parseMultiComment =
    parseStr "/*" *>
    ( MultiComm <$>
      parseTextUntil "*/" <*
      parseStr "*/"
    )

parseRead :: Parser Stmt
parseRead = Read <$>
    ( parseStr "read" *>
      parens parseName
    )

parsePrint :: Parser Stmt
parsePrint = Print <$>
    ( parseStr "print" *>
      parens parseExpr
    )

parseElse :: Parser Stmt
parseElse = parseStr "else" *> (parseBlockWrapped <|> parseBlock)

parseIf :: Parser Stmt
parseIf = If <$>
    parseCondWrapped "if" <*>
    parseBlockWrapped <*>
    optional parseElse

parseWhile :: Parser Stmt
parseWhile = While <$>
    parseCondWrapped "while" <*>
    parseBlockWrapped

parseFor :: Parser Stmt
parseFor =
    parseStr "for" *>
    parseStr "(" *>
    ( For <$>
      parseExpr <*
      parseStr ";" <*>
      parseExpr <*
      parseStr ";" <*>
      parseExpr <*
      parseStr ")" <*>
      parseBlockWrapped
    )

parseDefArg :: Parser (Type, String, Maybe Expr)
parseDefArg = do
    typ <- parseType
    name <- parseName
    expr <- parseDeclareAssignment
    return (typ , name, expr)

parseDefArgs :: Parser [(Type, String, Maybe Expr)]
parseDefArgs = do
    arg <- parseDefArg
    args <- (parseStr "," *> parseDefArgs) <|> pure []
    return (arg : args)

parseFuncDef :: Parser Stmt
parseFuncDef = FuncDef <$>
    parseType <*>
    parseName <*>
    parens parseDefArgs <*>
    parseBlockWrapped

parseReturn :: Parser Stmt
parseReturn = do
    parseStr "return"
    expr <- optional parseExpr
    return $ Return expr

parseSimpleStmt :: Parser Stmt
parseSimpleStmt
    =  parseReturn
   <|> parsePrint
   <|> parseRead
   <|> Continue <$ parseStr "continue"
   <|> Break <$ parseStr "break"
   <|> Expr <$> parseExpr

parseStmt :: Parser Stmt
parseStmt
    =  parseIf
   <|> parseWhile
   <|> parseFor
   <|> parseFuncDef
   <|> parseComment
   <|> parseMultiComment
   <|> (parseSimpleStmt <* parseChar ';')

parseBlock :: Parser Stmt
parseBlock = Seq <$> some parseStmt

parseBlockWrapped :: Parser Stmt
parseBlockWrapped = Seq <$> (parseStr "{" *> some parseStmt <* parseStr "}")

parseAST :: String -> Either ParseError Stmt
parseAST = parseWith
    ( do
      ast <- parseBlock
      parseEOF
      return ast
    )
