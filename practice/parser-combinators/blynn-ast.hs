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

data Expr
    = Num Integer
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Pow Expr Expr
    deriving (Show)

newtype Charser a = Charser { unCharser :: String -> Either String (a, String) }

sat :: (Char -> Bool) -> Charser Char
sat f = Charser $ \case
    h:t | f h -> Right (h, t)
    _ -> Left "unsat"

eof :: Charser ()
eof = Charser $ \case
    [] -> Right ((), "")
    _ -> Left "want EOF"

instance Functor Charser where
    fmap f (Charser x) = Charser $ fmap (first f) . x

instance Applicative Charser where
    pure a = Charser $ Right . (a,)
    f <*> x = Charser $ \s -> do
        (fun, t) <- unCharser f s
        (arg, u) <- unCharser x t
        pure (fun arg, u)

instance Monad Charser where
    return = pure
    Charser f >>= g = Charser $ (uncurry (unCharser . g) =<<) . f

instance Alternative Charser where
    empty = Charser $ const $ Left ""
    x <|> y = Charser $ \s -> either (const $ unCharser y s) Right $ unCharser x s

digitChar :: Charser Char
digitChar = sat $ \c -> '0' <= c && c <= '9'

char :: Char -> Charser Char
char = sat . (==)

oneOf :: String -> Charser Char
oneOf s = sat (`elem` s)

chainl1 :: Charser a -> Charser (a -> a -> a) -> Charser a
chainl1 p infixOp = go =<< p
  where
    go x = (go =<< ($ x) <$> infixOp <*> p) <|> pure x

chainr1 :: Charser a -> Charser (a -> a -> a) -> Charser a
chainr1 p infixOp = (&) <$> p <*> (flip <$> infixOp <*> chainr1 p infixOp <|> pure id)

sp :: Charser a -> Charser a
sp p = p <* many (oneOf " \n")

spch :: Char -> Charser Char
spch = sp . char

op :: Char -> (Expr -> Expr -> Expr) -> Charser (Expr -> Expr -> Expr)
op c f = spch c *> pure f

num :: Charser Expr
num = Num . parseInteger <$> sp (some digitChar)

atom :: Charser Expr
atom = num <|> spch '(' *> expr <* spch ')'

factor :: Charser Expr
factor = chainr1 atom (powOp)
  where
    powOp = op '^' Pow

term :: Charser Expr
term = chainl1 factor (mulOp <|> divOp <|> modOp)
  where
    mulOp = op '*' Mul
    divOp = op '/' Div
    modOp = op '%' Mod

expr :: Charser Expr
expr = chainl1 term (addOp <|> subOp)
  where
    addOp = op '+' Add
    subOp = op '-' Sub

line :: Charser Expr
line = sp (pure ()) *> expr <* eof

main :: IO ()
main = do
    let input = "3 + 5 * ( 2 - 8 )"
    let result = unCharser line input
    case result of
        Right (ast, "") -> print ast
        _ -> putStrLn "Parsing failed."
