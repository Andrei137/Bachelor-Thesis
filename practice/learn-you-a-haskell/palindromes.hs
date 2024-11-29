isPal :: String -> Bool
isPal s = s == reverse s

respondPalindromes :: String -> String
respondPalindromes = 
    unlines .
    map (\s -> (if isPal s then "" else "not a ") ++ "palindrome") .
    lines

main = interact respondPalindromes
