
    import System.IO

    main = do
        todoItem <- getLine
        appendFile "09_todo/todo.txt" $ todoItem ++ "\n"



