
    main = do
        interact respondPalindromes


    isPal :: String -> Bool
    isPal xs = xs == reverse xs

    respondPalindromes :: String -> String
    respondPalindromes =
        unlines
        . map ( \ xs -> if isPal xs 
                          then "palindrome"
                          else "not palindrome" )
        . lines


