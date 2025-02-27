-- https://apotheca.io/articles/haskell-parser.shtml

import Data.Char
import Control.Applicative
import Control.Monad
import Data.Bifunctor (first)

data ParseError
    = EndOfInput
    | InputRemaining String
    | Unexpected String
    | Failure String
    | Empty
    deriving (Show)

newtype Parser a = Parser { runParser :: String -> Either ParseError (a, String) }

parse :: Parser a -> String -> a
parse p s = case runParser p s of
    Left e        -> error $ show e
    Right (a, _)  -> a
    -- Right (a, []) -> a
    -- Right (a, s)  -> error "unexpected input remaining"

instance Functor Parser where
    fmap f p = Parser $ \s -> case runParser p s of
        Left e        -> Left e
        Right (a, s') -> Right (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    p <*> q = Parser $ \s -> case runParser p s of
        Left e        -> Left e
        Right (f, s') -> case runParser q s' of
            Left e         -> Left e
            Right (a, s'') -> Right (f a, s'')

instance Monad Parser where
    p >>= m = Parser $ \s -> case runParser p s of
        Left e        -> Left e
        Right (a, s') -> runParser (m a) s'

instance Alternative Parser where
    empty = Parser $ \s -> Left Empty
    p <|> q = Parser $ \s -> case runParser p s of
        Left _ -> runParser q s
        a      -> a

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    []     -> Left EndOfInput
    (c:cs) -> Right (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    []     -> Left EndOfInput
    (c:cs) -> if p c
        then Right (c, cs)
        else Left (Unexpected (c:[]))

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (flip elem cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (not . flip elem cs)

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

digit :: Parser Char
digit = satisfy isDigit

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

spaces :: Parser String
spaces = many $ oneOf " \n\r\t"

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

count :: Int -> Parser a -> Parser [a]
count n p
    | n <= 0    = return []
    | otherwise = sequence (replicate n p)

between :: Parser a -> Parser b -> Parser c -> Parser c
between o c a = do
    o
    a' <- a
    c
    pure a'

surround :: Parser a -> Parser b -> Parser b
surround q a = q *> a <* q

int :: Parser Int
int = read <$> some digit

op :: Parser (Int -> Int -> Int)
op = (char '+' >> return (+)) <|> (char '-' >> return (-))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= loop where
    loop x = (do
        f <- op
        y <- p
        loop (f x y)
        ) <|> return x

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = chainl1 p op <|> return a

comma :: Parser Char
comma = char ','

someSep :: Parser a -> Parser sep -> Parser [a]
someSep p sep = do
    x <- p
    xs <- many (sep >> p)
    return (x:xs)

manySep :: Parser a -> Parser sep -> Parser [a]
manySep p sep = someSep p sep <|> return []

newline :: Parser Char
newline = char '\n'

someEnd :: Parser a -> Parser sep -> Parser [a]
someEnd p sep = some $ do
    x <- p
    sep
    return x

manyEnd :: Parser a -> Parser sep -> Parser [a]
manyEnd p sep = many $ do
    x <- p
    sep
    return x

someSepEnd :: Parser a -> Parser sep -> Parser [a]
someSepEnd p sep = do
    x <- p
    (do
        sep
        xs <- manySepEnd p sep
        return (x:xs)
        ) <|> return [x]

manySepEnd :: Parser a -> Parser sep -> Parser [a]
manySepEnd p sep = someSepEnd p sep <|> return []

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Right ((), [])
    _  -> Left (InputRemaining s)
