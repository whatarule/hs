
    import System.Environment
    import System.Directory
    import System.IO
    import Data.List
    import Control.Exception

    dispatch :: String -> [ String ] -> IO ( )
    dispatch "add" = add
    dispatch "view" = view
    dispatch "remove" = remove
    dispatch "bump" = bump
    dispatch command = doesntExist command

    availableCommands :: IO ( )
    availableCommands = do
        let commands = [ "add", "view", "remove", "bump" ]
        putStrLn $ "Available commands:"
        mapM_ putStrLn commands

    doesntExist :: String -> [ String ] -> IO ( )
    doesntExist command _ = do
        putStrLn $ "The " ++ command ++ " command doesn't exits"
        availableCommands


    main = do
        args <- getArgs
        if length args == 0
        then do
            putStrLn "The program takes at least two arguments: a name of command and a file"
            availableCommands
        else do
            ( command : argList ) <- getArgs
            dispatch command argList


    add :: [ String ] -> IO ( )
    add [ fileName, todoItem ] =
        appendFile fileName ( todoItem ++ "\n" )
    add _ = putStrLn "The add command takes exactly two arguments"

    view :: [ String ] -> IO ( )
    view [ fileName ] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith
                ( \ n line -> show n ++ " - " ++ show line )
                [ 0.. ] todoTasks
        putStr $ unlines numberedTasks
    view _ = putStrLn "The view command takes exactly one arguments"

    remove :: [ String ] -> IO ( )
    remove [ fileName, numberString ] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith
                ( \ n line -> show n ++ " - " ++ show line )
                [ 0.. ] todoTasks
    --  putStrLn "These are your TO-DO items:"
    --  mapM_ putStrLn numberedTasks
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
                removeFile fileName
                renameFile tempName fileName
              )
    remove _ = putStrLn "The remove command takes exactly two arguments"

    bump :: [ String ] -> IO ( )
    bump [ fileName, numberString ] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith
                ( \ n line -> show n ++ " - " ++ show line )
                [ 0.. ] todoTasks
    --  putStrLn "These are your TO-DO items:"
    --  mapM_ putStrLn numberedTasks
        let number = read numberString
            newTodoItems = unlines
              $ todoTasks !! number
              : delete ( todoTasks !! number ) todoTasks
        bracketOnError ( openTempFile "." "temp" )
            ( \ ( tempName, tempHandle ) -> do
                hClose tempHandle
                removeFile tempName
              )
            ( \ ( tempName, tempHandle ) -> do
                hPutStr tempHandle newTodoItems
                hClose tempHandle
                removeFile fileName
                renameFile tempName fileName
              )
    bump _ = putStrLn "The bump command takes exactly two arguments"


