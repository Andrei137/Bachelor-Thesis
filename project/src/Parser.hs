module Parser (
    parseFile
) where

import Lexer
import AST.Base
import AST.Expr
import AST.Stmt
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

-- Maps from operators to constructors
mapAOp :: [(String, ABinOp)]
mapAOp = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("%", Mod)]

mapBOp :: [(String, BBinOp)]
mapBOp = [("and", And), ("or", Or), ("xor", Xor)]

mapROp :: [(String, RBinOp)]
mapROp = [("<", Lt), ("<=", Lte), (">", Gt), (">=", Gte), ("==", Eq), ("!=", Neq)]

-- Parsers
parseNeg :: Parser (AExpr -> AExpr)
parseNeg = reservedOp "-"   >> return (AUnary Neg)

parseNot :: Parser (BExpr -> BExpr)
parseNot = reservedOp "not" >> return (BUnary Not)

parseABinOp :: Parser (AExpr -> AExpr -> AExpr)
parseABinOp = choice $ map (\(op, constructor) -> reservedOp op >> return (ABinary constructor)) mapAOp

parseBBinOp :: Parser (BExpr -> BExpr -> BExpr)
parseBBinOp = choice $ map (\(op, constructor) -> reservedOp op >> return (BBinary constructor)) mapBOp

parseRBinOp :: Parser RBinOp
parseRBinOp = choice $ map (\(op, constructor) -> reservedOp op >> return constructor) mapROp

parseBoolConst :: Parser BExpr
parseBoolConst =  (reserved "true" >> return (BoolConst True))
              <|> (reserved "false" >> return (BoolConst False))

-- Arithmetic expression parser
aOperators :: OperatorTable Char () AExpr
aOperators = [ [Prefix parseNeg]
             , [Infix parseABinOp AssocLeft]
             ]

aTerm :: Parser AExpr
aTerm = parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bOperators :: OperatorTable Char () BExpr
bOperators = [ [Prefix parseNot]
             , [Infix parseBBinOp AssocLeft]
             ]

-- Boolean expression parser
bTerm :: Parser BExpr
bTerm = parens bExpression
     <|> parseBoolConst
     <|> rExpression

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

-- Relational expression parser
rExpression :: Parser BExpr
rExpression = do
    a1 <- aExpression
    op <- parseRBinOp
    a2 <- aExpression
    return $ RBinary op a1 a2

-- Statement parser
assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ Assign var expr

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    reserved "end"
    return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    reserved "end"
    return $ While cond stmt

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

statement' :: Parser Stmt
statement' = ifStmt
          <|> whileStmt
          <|> skipStmt
          <|> assignStmt
          <|> return Skip

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- sepBy1 statement' semi
    return $ Seq $ list

statement :: Parser Stmt
statement = parens statement
         <|> sequenceOfStmt

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

parseFile :: String -> IO Stmt
parseFile file = do
    program <- readFile file
    case parse whileParser "" program of
        Left e -> print e >> fail "parse error"
        Right r -> return r

