
    import System.IO
{-
    type FilePath = String
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    openFile : FilePath -> IOMode -> IO Handle
-}
{-
    import Control.Exception
    bracket :: IO a -> ( a -> IO b ) -> ( a -> IO c ) -> IO c

    withFile :: Filepath -> IOMode -> ( Handle -> IO a ) -> IO a
    withFile name mode f =
        bracket $ openFile name mode
            $ \ handle -> hClose handle
            $ \ handle -> f handle
-}

    main = do
        contents <- readFile "09_file/baabaa.txt"
        putStr contents
    {-
        withFile "09_file/baabaa.txt" ReadMode
            $ \ handle -> do
                contents <- hGetContents handle
                putStr contents
    -}
    {-
        handle <- openFile "09_file/baabaa.txt" ReadMode
        contents <- hGetContents handle
        putStr contents
        hClose handle
    -}
        


