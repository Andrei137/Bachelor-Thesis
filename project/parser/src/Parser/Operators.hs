module Parser.Operators
    ( operatorsTable
    , parseAssignOp
    ) where

import Control.Applicative
import AST.Operators
import AST.Expressions
import Parser.Core
import Parser.Primitives

parseNeg, parseInc, parseDec, parseNot :: Parser (Expr -> Expr)
parseNeg = parseOperator "-" $ UnaryOp Neg
parseInc = parseOperator "++" $ UnaryOp Inc
parseDec = parseOperator "--" $ UnaryOp Dec
parseNot = parseOperator "!" $ UnaryOp Not

parseAdd, parseSub, parseMul, parseDiv, parseMod, parsePow, parseAnd, parseOr,
  parseEq, parseNeq, parseLte, parseLt, parseGte, parseGt, parseConcat :: Parser (Expr -> Expr -> Expr)
parseAdd = parseOperator "+" $ BinaryOp Add
parseSub = parseOperator "-" $ BinaryOp Sub
parseMul = parseOperator "*" $ BinaryOp Mul
parseDiv = parseOperator "/" $ BinaryOp Div
parseMod = parseOperator "%" $ BinaryOp Mod
parsePow = parseOperator "**" $ BinaryOp Pow
parseAnd = parseOperator "&&" $ BinaryOp And
parseOr  = parseOperator "||" $ BinaryOp Or
parseEq  = parseOperator "==" $ BinaryOp Eq
parseNeq = parseOperator "!=" $ BinaryOp Neq
parseLte = parseOperator "<=" $ BinaryOp Lte
parseLt  = parseOperator "<"  $ BinaryOp Lt
parseGte = parseOperator ">=" $ BinaryOp Gte
parseGt  = parseOperator ">"  $ BinaryOp Gt
parseConcat = parseOperator "." $ BinaryOp Concat

operatorsTable :: OperatorsTable Expr
operatorsTable =
    [ [ Postfix parseInc
      , Postfix parseDec
      , Prefix parseNeg
      , Prefix parseNot
      ]
    , [ Infix RightAssoc parsePow
      ]
    , [ Infix LeftAssoc parseMul
      , Infix LeftAssoc parseDiv
      , Infix LeftAssoc parseMod
      ]
    , [ Infix LeftAssoc parseAdd
      , Infix LeftAssoc parseSub
      , Infix LeftAssoc parseConcat
      ]
    , [ Infix LeftAssoc parseEq
      , Infix LeftAssoc parseNeq
      , Infix LeftAssoc parseLt
      , Infix LeftAssoc parseLte
      , Infix LeftAssoc parseGt
      , Infix LeftAssoc parseGte
      ]
    , [ Infix LeftAssoc parseAnd
      , Infix LeftAssoc parseOr
      ]
    ]

parseAssignOp :: Parser AssignOp
parseAssignOp
    = parseOperator ":=" Basic
   <|> parseOperator "+=" (With Add)
   <|> parseOperator "-=" (With Sub)
   <|> parseOperator "*=" (With Mul)
   <|> parseOperator "/=" (With Div)
   <|> parseOperator "%=" (With Mod)
   <|> parseOperator "**=" (With Pow)
   <|> parseOperator ".=" (With Concat)
