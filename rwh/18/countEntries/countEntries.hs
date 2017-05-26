
--  module CountEntries where
--  module CountEntriesT where

    import System.Directory
    import System.FilePath
    import Control.Monad
    import Control.Monad.Trans
    import Control.Monad.Writer


    listDirectory :: FilePath -> IO [ String ]
    listDirectory =
        liftM ( filter notDots ) . getDirectoryContents
        where notDots p = p /= "." && p /= ".."

    countEntriesTrad :: FilePath -> IO [ ( FilePath, Int ) ]
    countEntriesTrad path = do
        contents <- listDirectory path
        rest <- forM contents $ \ name -> do
                    let newName = path </> name
                    isDir <- doesDirectoryExist newName
                    if isDir
                        then countEntriesTrad newName
                        else return [ ]
        return $ ( path, length contents ) : concat rest

    countEntries :: FilePath -> WriterT [ ( FilePath, Int ) ] IO ( )
    countEntries path = do
        contents <- liftIO . listDirectory $ path
        tell [ ( path, length contents ) ]
        forM_ contents $ \ name -> do
            let newName = path </> name
            isDir <- liftIO . doesDirectoryExist $ newName
            when isDir $ countEntries newName

    
    main = do
        
        r <- countEntriesTrad "18/countEntries"
        print r

        r <- runWriterT $ countEntries "18/countEntries"
        print r
        r <- execWriterT $ countEntries "18/countEntries"
        print r
        r <- take 3 `liftM` execWriterT ( countEntries ".." )
        print r





