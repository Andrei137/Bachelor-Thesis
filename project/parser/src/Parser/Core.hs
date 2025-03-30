module Parser.Core
    ( ParseError(..)
    , Parser(..)
    , Associative(..)
    , Operator(..)
    , OperatorsTable
    , expressionParser
    , parseWith
    ) where

import Control.Applicative

data ParseError
    = Empty
    | Failure String
    deriving (Show)

newtype Parser a = Parser
    { runParser :: String -> Either ParseError (a, String)
    }

data Associative = LeftAssoc | RightAssoc
    deriving (Show)

data Operator a
    = Prefix (Parser (a -> a))
    | Infix Associative (Parser (a -> a -> a))
    | Postfix (Parser (a -> a))

type OperatorsTable a = [[Operator a]]

instance Functor Parser where
    fmap f (Parser pa) = Parser $ \input -> fmap applyF (pa input)
        where applyF (a, rest) = (f a, rest)

instance Applicative Parser where
    pure a = Parser $ \input -> Right (a, input)
    (Parser pf) <*> (Parser pa) = Parser $ \input -> do
        (f, output) <- pf input
        (a, output') <- pa output
        pure (f a, output')

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \input -> do
        (a, output) <- pa input
        runParser (f a) output

instance Alternative Parser where
    empty = Parser $ \_ -> Left Empty
    (Parser pf) <|> (Parser pg) = Parser $ \input -> case pf input of
        Left _ -> pg input
        result -> result

expressionParser :: OperatorsTable a -> Parser a -> Parser a
expressionParser operatorsTable parseTerm = foldl buildParser parseTerm operatorsTable
    where
        buildParser p ops = foldl (flip applyOp) p ops

        applyOp (Prefix op) p = op <*> p <|> p
        applyOp (Infix LeftAssoc op) p = p `chainl1` op
        applyOp (Infix RightAssoc op) p = p `chainr1` op
        applyOp (Postfix op) p = (p >>= \x -> op <*> pure x) <|> p

        p `chainl1` op = p >>= rest
            where
                rest x =
                    (do f <- op
                        y <- p
                        rest (f x y)) <|> return x
        p `chainr1` op =
            (do x <- p
                f <- op
                y <- p `chainr1` op
                return (f x y)) <|> p

parseWith :: Parser a -> String -> Either ParseError a
parseWith (Parser p) s = case p s of
    Right (a, []) -> Right a
    Right _ -> Left $ Failure $ "Something went wrong"
    Left e -> Left e
