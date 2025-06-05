{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Parser.Primitives
    ( parseEOF
    , parseChar
    , parseStr
    , parseName
    , parseIdentifier
    , parseAnyChar
    , parseAnyText
    , parseTextUntil
    , parseInteger
    , parseDouble
    , parseBool
    , parseOperator
    , parens
    , sepBy
    ) where

import Control.Monad
import Data.Char
import Control.Applicative
import Parser.Core

parseEOF :: Parser ()
parseEOF = Parser $ \case
    [] -> Right ((), "")
    _ -> Left $ Failure "End of file expected"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
    h:t -> (if p h then Right (h, t) else Left $ Failure $ "Unexpected character: " ++ [h] ++ " found in " ++ t)
    _ -> Left $ Failure "Expected a character but got none"

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = traverse char

alphaChar :: Parser Char
alphaChar = satisfy isAlpha

identifierChar :: Parser Char
identifierChar = satisfy (\c -> isAlphaNum c || c == '_')

digitChar :: Parser Char
digitChar = satisfy isDigit

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

token :: Parser a -> Parser a
token p = p <* many (oneOf " \n\t\r")

parseChar :: Char -> Parser Char
parseChar = token . char

parseStr :: String -> Parser ()
parseStr = void . token . string

parseName :: Parser String
parseName = token $ do
    first <- alphaChar
    rest <- many identifierChar
    return (first : rest)

parseIdentifier :: String -> Parser String
parseIdentifier = token . string

parseAnyChar :: Parser Char
parseAnyChar = token (parseChar '\'' *> satisfy (/= '\'') <* parseChar '\'')

parseAnyText :: Parser String
parseAnyText = token (parseChar '"' *> many (satisfy (/= '"')) <* parseChar '"')

parseTextUntil :: String -> Parser String
parseTextUntil sep = token (many (satisfy (`notElem` sep)))

parseInteger :: Parser Integer
parseInteger = parseInteger' <$> token (some digitChar)
    where parseInteger' = foldl (\n d -> 10 * n + fromIntegral (ord d - ord '0')) 0

parseDouble :: Parser Double
parseDouble = token $ do
    whole <- some digitChar
    (do
        parseStr "."
        fractional <- some digitChar
        return (read (whole ++ "." ++ fractional)))

parseBool :: Parser Bool
parseBool = token (True <$ parseStr "true" <|> False <$ parseStr "false")

parseOperator :: String -> a -> Parser a
parseOperator op f = f <$ parseStr op

parens :: Parser a -> Parser a
parens p = parseChar '(' *> p <* parseChar ')'

sepBy :: Parser a -> String -> Parser [a]
sepBy p sep =
    (do
        first <- p
        rest <- many (do
            parseStr sep
            p
          )
        return (first : rest)
    )
    <|> return []
