{-# LANGUAGE DeriveGeneric     #-}

module AST.Types
    ( Type(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))

data Type
    = IntT
    | DoubleT
    | BoolT
    | CharT
    | StringT
    | VoidT
    deriving (Show, Eq, Generic)

instance ToJSON Type
