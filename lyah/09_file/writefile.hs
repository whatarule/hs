
    import System.IO
    import Data.Char

    main = do
        contents <- readFile "09_file/baabaa.txt"
        writeFile "09_file/baabaacaps.txt"
            $ map toUpper contents



