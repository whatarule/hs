
    import System.IO
    import System.Directory
    import System.IO.Error
    import Control.Exception


    main :: IO ( )
    main = withTempFile "tempFile/mytemp.txt" myAction


    myAction :: FilePath -> Handle -> IO ( )
    myAction tempname temph = do
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname

        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        let tempdata = show [ 1..10 ]
        pusStrLn $
            "Writing one line containing " ++
            show ( length tempdata ) ++ " bytes: " ++
            tempdata
        hPutStrLn temph tempdata


