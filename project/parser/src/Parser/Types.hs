module Parser.Types
    ( parseType
    ) where

import Control.Applicative
import Data.Functor
import AST.Types
import Parser.Core
import Parser.Primitives

parseType :: Parser Type
parseType
    =  parseStr "int" $> IntT
   <|> parseStr "double" $> DoubleT
   <|> parseStr "bool" $> BoolT
   <|> parseStr "char" $> CharT
   <|> parseStr "string" $> StringT
   <|> parseStr "void" $> VoidT
