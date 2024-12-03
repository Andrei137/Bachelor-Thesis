module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

{-
    opa ::= + | - | * | /
    opb ::= and | or
    opr ::= > | <
    a   ::= x | n | - a | a opa a
    b   ::= true | false | not b | b opb b | a opr a
    S   ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
-}

-- opa (arithmetic operators)
data ABinOp = Add | Subtract | Multiply | Divide deriving (Show)

-- opb (boolean operators)
data BBinOp = And | Or deriving (Show)

-- opr (relational operators)
data RBinOp = Greater | Less deriving (Show)

-- a (arithmetic expressions)
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

-- b (boolean expressions)
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

-- S (statements)
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)

languageDef = emptyDef
    { Token.commentStart    = "/"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum
    , Token.reservedNames   =
        [ "if", "then", "else"
        , "while", "do", "skip"
        , "true", "false", "not"
        , "and", "or"
        ]
    , Token.reservedOpNames =
        [ "+", "-", "*", "/", ":="
        , "<", ">", "and", "or", "not"
        ]

    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved    = Token.reserved   lexer
reservedOp  = Token.reservedOp lexer
parens      = Token.parens     lexer
integer     = Token.integer    lexer
semi        = Token.semi       lexer
whiteSpace  = Token.whiteSpace lexer

parseNeg   = reservedOp "-"   >> return Neg
parseAdd   = reservedOp "+"   >> return (ABinary Add)
parseSub   = reservedOp "-"   >> return (ABinary Subtract)
parseMul   = reservedOp "*"   >> return (ABinary Multiply)
parseDiv   = reservedOp "/"   >> return (ABinary Divide)
parseNot   = reservedOp "not" >> return Not
parseAnd   = reservedOp "and" >> return (BBinary And)
parseOr    = reservedOp "or"  >> return (BBinary Or)
parseGt    = reservedOp ">"   >> return Greater
parseLt    = reservedOp "<"   >> return Less
parseTrue  = reserved "true"  >> return (BoolConst True)
parseFalse = reserved "false" >> return (BoolConst False)

aOperators = [ [Prefix parseNeg]
             , [Infix parseMul AssocLeft, Infix parseDiv AssocLeft]
             , [Infix parseAdd AssocLeft, Infix parseSub AssocLeft]
             ]

bOperators = [ [Prefix parseNot]
             , [Infix parseAnd AssocLeft, Infix parseOr AssocLeft]
             ]

aTerm = parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm = parens bExpression
     <|> parseTrue
     <|> parseFalse
     <|> rExpression

relation = parseGt <|> parseLt

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

rExpression :: Parser BExpr
rExpression = do
    a1 <- aExpression
    op <- relation
    a2 <- aExpression
    return $ RBinary op a1 a2

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    return $ While cond stmt

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ Assign var expr

statement' :: Parser Stmt
statement' =  ifStmt
          <|> whileStmt
          <|> skipStmt
          <|> assignStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- sepBy1 statement' semi
    return $ Seq list

statement :: Parser Stmt
statement = parens statement <|> try sequenceOfStmt

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

parseFile :: String -> IO Stmt
parseFile file = do
    program <- readFile file
    case parse whileParser "" program of
        Left e -> print e >> fail "parse error"
        Right r -> return r

reconstructOpA :: ABinOp -> String
reconstructOpA Add = "+"
reconstructOpA Subtract = "-"
reconstructOpA Multiply = "*"
reconstructOpA Divide = "/"

reconstructOpR :: RBinOp -> String
reconstructOpR Greater = ">"
reconstructOpR Less = "<"

reconstructA :: AExpr -> String
reconstructA (Var var) = var
reconstructA (IntConst n) = show n
reconstructA (Neg expr) = "-" ++ reconstructA expr
reconstructA (ABinary op expr1 expr2) =
    reconstructA expr1 ++ " " ++
    reconstructOpA op ++ " " ++
    reconstructA expr2

reconstructB :: BExpr -> String
reconstructB (BoolConst b) = show b
reconstructB (Not expr) = "not " ++ reconstructB expr
reconstructB (BBinary And expr1 expr2) =
    reconstructB expr1 ++ " and " ++
    reconstructB expr2
reconstructB (BBinary Or expr1 expr2) =
    reconstructB expr1 ++ " or " ++
    reconstructB expr2
reconstructB (RBinary op expr1 expr2) =
    reconstructA expr1 ++ " " ++
    reconstructOpR op ++ " " ++
    reconstructA expr2

reconstruct :: Stmt -> String
reconstruct (Assign var expr) =
    var ++ " := " ++
    reconstructA expr
reconstruct Skip = "skip"
reconstruct (If cond stmt1 stmt2) =
    "if " ++ reconstructB cond ++
    " then\n\t" ++ reconstruct stmt1 ++
    "\nelse\n\t" ++ reconstruct stmt2
reconstruct (While cond stmt) =
    "while " ++ reconstructB cond ++
    " do\n\t" ++ reconstruct stmt
reconstruct (Seq []) = ""
reconstruct (Seq [stmt]) = reconstruct stmt
reconstruct (Seq (stmt:stmts)) = reconstruct stmt ++ ";\n" ++ reconstruct (Seq stmts)

main :: IO ()
main = do
    let inputFile = "txt/in.while"
    let astFile = "txt/ast.txt"
    let outputFile = "txt/out.while"
    ast <- parseFile inputFile
    writeFile astFile $ show ast
    writeFile outputFile $ reconstruct ast
