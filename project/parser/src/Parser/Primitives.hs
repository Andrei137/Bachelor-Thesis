{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Parser.Primitives
    ( parseEOF
    , parseChar
    , parseStr
    , parseName
    , parseText
    , parseNumber
    , parseBool
    , parseOperator
    , parens
    ) where

import Data.Char
import Control.Applicative
import Parser.Core

parseEOF :: Parser ()
parseEOF = Parser $ \case
    [] -> Right ((), "")
    _ -> Left $ Failure "End of file expected"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
    h:t -> case p h of
        True -> Right (h, t)
        False -> Left $ Failure $ "Unexpected character: " ++ [h] ++ " found in " ++ t
    _ -> Left $ Failure "Expected a character but got none"

anyChar :: Parser Char
anyChar = satisfy (const True)

anyString :: Parser String
anyString = many anyChar

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = traverse char

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum

digitChar :: Parser Char
digitChar = satisfy isDigit

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

token :: Parser a -> Parser a
token p = p <* many (oneOf " \n\t\r")

parseChar :: Char -> Parser Char
parseChar = token . char

parseStr :: String -> Parser ()
parseStr = fmap (const ()) . token . string

parseName :: Parser String
parseName = token (some alphaNumChar)

parseText :: Parser String
parseText = token (parseChar '"' *> many (satisfy (/= '"')) <* parseChar '"')

parseNumber :: Parser Integer
parseNumber = parseInteger <$> token (some digitChar)
    where parseInteger = foldl (\n d -> 10 * n + fromIntegral (ord d - ord '0')) 0

parseBool :: Parser Bool
parseBool = token (True <$ parseStr "true" <|> False <$ parseStr "false")

parseOperator :: String -> a -> Parser a
parseOperator op f = f <$ parseStr op

parens :: Parser a -> Parser a
parens p = parseChar '(' *> p <* parseChar ')'
