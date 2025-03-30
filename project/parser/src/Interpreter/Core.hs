module Interpreter.Core
    ( Value(..)
    , Env
    , Interpreter
    , find
    , set
    , showValue
    , interpretWith
    ) where

import Control.Monad.State (StateT, runStateT, get, modify)
import qualified Data.Map as Map (Map, empty, lookup, insert)

data Value
    = IntVal Integer
    | BoolVal Bool
    deriving (Show)
type Env = Map.Map String Value
type Interpreter = StateT Env IO

find :: String -> Interpreter Value
find var = do
    env <- get
    case Map.lookup var env of
        Just v -> return v
        Nothing -> error $ "Variable " ++ var ++ " not found"

set :: String -> Value -> Interpreter ()
set var val = modify $ Map.insert var val

showValue :: Value -> String
showValue (IntVal i) = show i
showValue (BoolVal b) = show b

interpretWith :: (a -> Interpreter ()) -> a -> IO ()
interpretWith evalS s = do
    _ <- runStateT (evalS s) Map.empty
    return ()
