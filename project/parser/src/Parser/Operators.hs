module Parser.Operators
    ( arithOperatorsTable
    , boolOperatorsTable
    , parseRelOp
    , parseAssignOp
    ) where

import Control.Applicative
import AST.Types.Operators
import AST.Types.Expressions
import Parser.Core
import Parser.Primitives

-- Arihtmetic
parseNeg, parseInc, parseDec :: Parser (AExpr -> AExpr)
parseNeg = parseOperator "-" $ AUnary Neg
parseInc = parseOperator "++" $ AUnary Inc
parseDec = parseOperator "--" $ AUnary Dec

parseAdd, parseSub, parseMul, parseDiv, parseMod, parsePow :: Parser (AExpr -> AExpr -> AExpr)
parseAdd = parseOperator "+" $ ABinary Add
parseSub = parseOperator "-" $ ABinary Sub
parseMul = parseOperator "*" $ ABinary Mul
parseDiv = parseOperator "/" $ ABinary Div
parseMod = parseOperator "%" $ ABinary Mod
parsePow = parseOperator "**" $ ABinary Pow

arithOperatorsTable :: OperatorsTable AExpr
arithOperatorsTable =
    [ [ Prefix parseNeg
      ]
    , [ Postfix parseInc
      , Postfix parseDec
      ]
    , [ Infix RightAssoc parsePow
      ]
    , [ Infix LeftAssoc parseMul
      , Infix LeftAssoc parseDiv
      , Infix LeftAssoc parseMod
      ]
    , [ Infix LeftAssoc parseAdd
      , Infix LeftAssoc parseSub
      ]
    ]

-- Boolean
parseNot :: Parser (BExpr -> BExpr)
parseNot = parseOperator "!" $ BUnary Not

parseAnd, parseOr :: Parser (BExpr -> BExpr -> BExpr)
parseAnd = parseOperator "&&" $ BBinary And
parseOr  = parseOperator "||" $ BBinary Or

boolOperatorsTable :: OperatorsTable BExpr
boolOperatorsTable =
    [ [ Prefix parseNot
      ]
    , [ Infix LeftAssoc parseAnd
      , Infix LeftAssoc parseOr
      ]
    ]

-- Relational
parseRelOp :: Parser RBinOp
parseRelOp
    =  parseOperator "==" Eq
   <|> parseOperator "!=" Neq
   <|> parseOperator "<=" Lte
   <|> parseOperator "<"  Lt
   <|> parseOperator ">=" Gte
   <|> parseOperator ">"  Gt

-- Assignment
parseAssignOp :: Parser AssignOp
parseAssignOp
    = parseOperator ":=" (Basic)
   <|> parseOperator "+=" (With Add)
   <|> parseOperator "-=" (With Sub)
   <|> parseOperator "*=" (With Mul)
   <|> parseOperator "/=" (With Div)
   <|> parseOperator "%=" (With Mod)
   <|> parseOperator "**=" (With Pow)
