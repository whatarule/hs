
    import System.IO
    import System.Directory
    import Data.List
    import Control.Exception
    import System.Environment

    main = do
        let filePath = "09_todo/todo.txt"
        contents <- readFile filePath
        let todoTasks = lines contents
            numberedTasks = zipWith 
                              ( \ n line -> show n ++ " - " ++ show line )
                              [ 0.. ] todoTasks
        putStrLn "These are your TO-DO items:"
        mapM_ putStrLn numberedTasks
        putStrLn "Which one do you want to delete?"
        numberString <- getLine
        let number = read numberString
            newTodoItems = unlines $ delete ( todoTasks !! number ) todoTasks
        bracketOnError ( openTempFile "." "temp" )
            ( \ ( tempName, tempHandle ) -> do
                hClose tempHandle
                removeFile tempName
              )
            ( \ ( tempName, tempHandle ) -> do
                hPutStr tempHandle newTodoItems
                hClose tempHandle
                removeFile tempName
                renameFile tempName filePath
              )
    {-
        ( tempName, tempHandle ) <- openTempFile "." "temp"
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile filePath
        renameFile tempName filePath
    -}


