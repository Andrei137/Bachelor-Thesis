module Parser.Operators
    ( operatorsTable
    , parseAssignOp
    ) where

import Data.Functor
import Control.Applicative
import AST.Operators
import AST.Expressions
import Parser.Core
import Parser.Primitives

parseNeg, parseInc, parseDec, parseNot :: Parser (Expr -> Expr)
parseNeg = parseStr "-" $> UnaryOp Neg
parseInc = parseStr "++" $> UnaryOp Inc
parseDec = parseStr "--" $> UnaryOp Dec
parseNot = parseStr "!" $> UnaryOp Not

parseAdd, parseSub, parseMul, parseDiv, parseMod, parsePow, parseAnd, parseOr,
  parseEq, parseNeq, parseLte, parseLt, parseGte, parseGt, parseConcat :: Parser (Expr -> Expr -> Expr)
parseAdd = parseStr "+" $> BinaryOp Add
parseSub = parseStr "-" $> BinaryOp Sub
parseMul = parseStr "*" $> BinaryOp Mul
parseDiv = parseStr "/" $> BinaryOp Div
parseMod = parseStr "%" $> BinaryOp Mod
parsePow = parseStr "^" $> BinaryOp Pow
parseAnd = parseStr "&&" $> BinaryOp And
parseOr  = parseStr "||" $> BinaryOp Or
parseEq  = parseStr "==" $> BinaryOp Eq
parseNeq = parseStr "!=" $> BinaryOp Neq
parseLte = parseStr "<=" $> BinaryOp Lte
parseLt  = parseStr "<"  $> BinaryOp Lt
parseGte = parseStr ">=" $> BinaryOp Gte
parseGt  = parseStr ">"  $> BinaryOp Gt
parseConcat = parseStr "." $> BinaryOp Concat

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
    = parseStr ":=" $> Basic
   <|> parseStr "+=" $> With Add
   <|> parseStr "-=" $> With Sub
   <|> parseStr "*=" $> With Mul
   <|> parseStr "/=" $> With Div
   <|> parseStr "%=" $> With Mod
   <|> parseStr "**=" $> With Pow
   <|> parseStr ".=" $> With Concat
