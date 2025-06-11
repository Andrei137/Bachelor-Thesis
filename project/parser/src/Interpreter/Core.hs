module Interpreter.Core
    ( Value(..)
    , Env
    , Interpreter
    , find
    , optionalFind
    , set
    , showValue
    , interpretWith
    , typeMatches
    ) where

import Control.Monad.State (StateT, runStateT, get, gets, modify)
import qualified Data.Map as Map (Map, empty, lookup, insert)

data Value
    = IntVal Integer
    | DoubleVal Double
    | BoolVal Bool
    | CharVal Char
    | StringVal String
    | VoidVal
    deriving (Show)
type Env = Map.Map String Value
type Interpreter = StateT Env IO

find :: String -> Interpreter Value
find var = do
    env <- get
    case Map.lookup var env of
        Just v -> return v
        Nothing -> error $ "Variable " ++ var ++ " not found"

optionalFind :: String -> Interpreter (Maybe Value)
optionalFind var = gets (Map.lookup var)

set :: String -> Value -> Interpreter ()
set var val = modify $ Map.insert var val

showValue :: Value -> String
showValue (IntVal i) = show i
showValue (DoubleVal d) = show d
showValue (BoolVal b) = show b
showValue (CharVal c) = show c
showValue (StringVal s) = s
showValue VoidVal = ""

typeMatches :: Value -> Value -> Bool
typeMatches (IntVal _) (IntVal _) = True
typeMatches (DoubleVal _) (DoubleVal _) = True
typeMatches (BoolVal _) (BoolVal _) = True
typeMatches (CharVal _) (CharVal _) = True
typeMatches (StringVal _) (StringVal _) = True
typeMatches VoidVal VoidVal = True
typeMatches _ _ = False

interpretWith :: (a -> Interpreter ()) -> a -> IO ()
interpretWith evalS s = do
    _ <- runStateT (evalS s) Map.empty
    return ()
