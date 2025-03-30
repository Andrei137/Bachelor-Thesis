module AST.Formatter.Core
    ( indent
    , formatWith
    ) where

import Control.Monad.Reader (Reader, runReader, ask)

indent :: Reader Int String
indent = do
    level <- ask
    return $ replicate (level * 4) ' '

formatWith :: (a -> Reader Int String) -> a -> String
formatWith formatS s = runReader (formatS s) 0
