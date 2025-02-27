-- https://serokell.io/blog/parser-combinators-in-haskell

import Control.Applicative (Alternative (..))
import Data.List (nub)

data Error i e
    = EndOfInput
    | Unexpected i
    | ExpectedEndOfFile i
    | CustomError e
    | Expected i i
    | Empty
    deriving (Eq, Show)

{-
    i = the input stream
    e = type of custom error message
    a = the result
-}
newtype Parser i e a = Parser
    { runParser :: [i] -> Either [Error i e] (a, [i])
    }

instance Functor (Parser i e) where
    fmap f (Parser p) = Parser $ \input -> do
        (output, rest) <- p input
        pure (f output, rest)

instance Applicative (Parser i e) where
    pure a = Parser $ \input -> Right $ (a, input)

    (Parser f) <*> (Parser x) = Parser $ \input -> do
        (f', rest) <- f input
        (output, rest') <- x rest
        pure (f' output, rest')

instance Monad (Parser i e) where
    return = pure

    (Parser p) >>= f = Parser $ \input -> do
        (output, rest) <- p input
        runParser (f output) rest

instance (Eq i, Eq e) => Alternative (Parser i e) where
    empty = Parser $ \_ -> Left [Empty]

    Parser l <|> Parser r = Parser $ \input ->
        case l input of
            Left err ->
                case r input of
                    Left err' -> Left $ nub $ err <> err'
                    Right (output, rest) -> Right (output, rest)
            Right (output, rest) -> Right (output, rest)


token :: (i -> Error i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input ->
    case input of
        [] -> Left [EndOfInput]
        hd : rest
            | predicate hd -> Right (hd, rest)
            | otherwise    -> Left [mkErr hd]

satisfy :: (i -> Bool) -> Parser i e i
satisfy = token Unexpected

char :: Eq i => i -> Parser i e i
char i = token (Expected i) (== i)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

eof :: Parser i e ()
eof = Parser $ \input ->
    case input of
        []   -> Right ((), [])
        hd : _  -> Left [ExpectedEndOfFile hd]
