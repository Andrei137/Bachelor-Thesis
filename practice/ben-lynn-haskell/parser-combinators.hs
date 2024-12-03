{-# LANGUAGE LambdaCase #-}

import Data.Char

fromDigit :: Char -> Int
fromDigit c = ord c - ord '0'

parseInteger :: String -> Integer
parseInteger = foldl (\n d -> 10*n + fromIntegral (fromDigit d)) 0

{-
    parseInteger "123" + parseInteger "456"
-}

parseDigit :: String -> Either String (Int, String)
parseDigit = \case
  c:s' | '0' <= c, c <= '9' -> Right (fromDigit c, s')
  _ -> Left "want digit"
