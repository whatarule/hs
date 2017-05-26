
    main = do
    {-
        contents <- getContents
        putStr $ shortLinesOnly contents
    -}
        interact shortLinesOnly

    shortLinesOnly :: String -> String
    shortLinesOnly = unlines
                      . filter ( \ line -> length line < 10 )
                      . lines

