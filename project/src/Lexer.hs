module Lexer (
    whiteSpace,
    identifier,
    reserved,
    reservedOp,
    integer,
    parens,
    semi
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- The language definition and lexer
languageDef :: Token.LanguageDef ()
languageDef = emptyDef
    { Token.commentStart    = "/-"
    , Token.commentEnd      = "-/"
    , Token.commentLine     = "--"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum
    , Token.reservedNames   =
        [ "if", "then", "else", "while", "do", "skip"
        , "end", "true", "false", "not", "and", "or"
        ]
    , Token.reservedOpNames =
        [ "+", "-", "*", "/"
        , "%", ":=", "not", "and"
        , "or", "xor", "<", "<="
        , ">", ">=", "==", "/="
        ]
    }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

-- Specific tokens
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semi :: Parser String
semi = Token.semi lexer