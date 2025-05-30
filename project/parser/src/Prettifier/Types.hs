module Prettifier.Types
    ( convertType
    ) where

import AST.Types

convertType :: Type -> String
convertType IntT = "int"
convertType DoubleT = "double"
convertType BoolT = "bool"
convertType CharT = "char"
convertType StringT = "string"
convertType VoidT = "void"
