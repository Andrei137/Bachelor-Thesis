-- https://theory.stanford.edu/~blynn/haskell/parse.html

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import GHC.Base hiding (foldr)
import Data.Char
import Data.Bifunctor (first)

(&) :: a -> (a -> b) -> b
(&) = flip ($)

fromDigit :: Char -> Int
fromDigit c = ord c - ord '0'

parseInteger :: String -> Integer
parseInteger = foldl (\n d -> 10 * n + fromIntegral (fromDigit d)) 0

parseDigit = \case
    c:s' | '0' <= c, c <= '9' -> Right (fromDigit c, s')
    _ -> Left "want digit"

parseTwoDigits s = do
    (a, s') <- parseDigit s
    (b, s'') <- parseDigit s'
    pure (10 * a + b, s'')

data Charser a = Charser { unCharser :: String -> Either String (a, String) }

sat f = Charser $ \case
    h:t | f h -> Right (h, t)
    _ -> Left "unsat"

eof = Charser $ \case
    [] -> Right ((), "")
    _ -> Left "want EOF"

{-
    fmap :: (a -> b) -> Charser a -> Charser b
    x :: String -> Either String (a, String)
    f :: a -> b
    first f :: (a, String) -> (b, String)
    fmap (first f) :: Either String (a, String) -> Either String (b, String)
    (fmap (first f) . x) :: String -> Either String (b, String)
-}
instance Functor Charser where
    fmap f (Charser x) = Charser $ fmap (first f) . x

instance Applicative Charser where
    pure a = Charser $ Right . (a,)
    f <*> x = Charser $ \s -> do
        (fun, t) <- unCharser f s
        (arg, u) <- unCharser x t
        pure (fun arg, u)

{-
    >>= :: Charser a -> (a -> Charser b) -> Charser b
    f :: String -> Either String (a, String)
    g :: a -> Charser b
    unCharser . g :: a -> String -> Either String (b, String)
    uncurry (unCharser . g) :: (a, String) -> Either String (b, String)
    =<< :: ((a, String) -> Either String (b, String)) -> Either String (a, String) -> Either String (b, String)
    (uncurry (unCharser . g) =<<) . f :: String -> Either String (b, String)
-}
instance Monad Charser where
    return = pure
    Charser f >>= g = Charser $ (uncurry (unCharser . g) =<<) . f

{-
    <|> :: Charser a -> Charser a -> Charser a
    unCharser x/y :: String -> Either String (a, String)
    unCharser x/y s :: Either String (a, String)
    const $ unCharser y s :: String -> Either String (a, String)
    Right :: (a, String) -> Either String (a, String)
    either (const $ unCharser y s) Right :: Either String (a, String) -> Either String (a, String)
-}
instance Alternative Charser where
    empty = Charser $ const $ Left ""
    x <|> y = Charser $ \s -> either (const $ unCharser y s) Right $ unCharser x s

digitChar = sat $ \c -> '0' <= c && c <= '9'

twoDigits = do
    a <- digitChar
    b <- digitChar
    pure $ 10 * fromDigit a + fromDigit b

char :: Char -> Charser Char
char = sat . (==)

oneOf :: String -> Charser Char
oneOf s = sat (`elem` s)

-- parses left-associated infix operators
chainl1 :: Charser a -> Charser (a -> a -> a) -> Charser a
chainl1 p infixOp = go =<< p where
    go x = (go =<< ($ x) <$> infixOp <*> p) <|> pure x

-- parses right-associated infix operators
chainr1 :: Charser a -> Charser (a -> a -> a) -> Charser a
chainr1 p infixOp = (&) <$> p <*> (flip <$> infixOp <*> chainr1 p infixOp <|> pure id)

sp :: Charser a -> Charser a
sp p = p <* many (oneOf " \n")

spch = sp . char

op c f = spch c *> pure f

{-
    num    ::= ('0'|..|'9'|'.')+
    atm    ::= num | '(' expr ')'
    pow    ::= atm ('^' atm )*
    una    ::= ('+'|'-')*
    fac    ::= una pow
    term   ::= fac ( ('*'|'/'|'%') fac )*
    expr   ::= term ( ('+'|'-') term )*
-}
line = sp (pure ()) *> expr <* eof where
    num = parseInteger <$> sp (some digitChar)
    atm = num <|> spch '(' *> expr <* spch ')'
    pow = chainr1 atm $ op '^' (^)
    una = foldr (.) id <$> many (op '+' id <|> op '-' negate)
    fac = una <*> pow
    term = chainl1 fac $ op '*' (*) <|> op '/' div <|> op '%' mod
    expr = chainl1 term $ op '+' (+) <|> op '-' (-)

-- "3 + 5 * (2 - 8)"
main = print $ unCharser line "3 + 5 * (2 - 8)"