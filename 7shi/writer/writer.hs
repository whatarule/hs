
    import Control.Monad.State
    import Control.Monad.Writer

    main = do

        putStrLn ""
        
        putStrLn "-- // Writer"
    --  Writer w a
        putStrLn ""
        
    -- // tell
        putStrLn "// tell"
    --  tell :: w -> Writer w ( )
    --  cf. modify :: ( s -> s ) -> State s ( ) 
        
    -- // runWriter
        putStrLn "// runWriter"
    --  runWriter :: Writer w a -> ( a, w )
    --  cf. runState :: State s a -> s -> ( a, s )
        
        print $ ( `runState` "" ) $ do
            modify ( ++ "a" )
            modify ( ++ "b" )
            modify ( ++ "c" )
            return ( )
        
        print $ runWriter $ do
            tell "a"
            tell "b"
            tell "c"
            return ( )

        putStrLn ""
    
    -- // fact
        putStrLn "// fact"

        let ( a, w ) = runWriter $ factWt 5
        putStr $ unlines w
        print $ a
        
        putStrLn ""

        print =<< factIO 5 

        putStrLn ""
        
    -- // writer
        putStrLn "// writer"
    --  writer :: ( a, w ) -> Writer w a
    --  cf. state :: ( s -> ( a, s ) ) -> State s a
        let a = writer ( 1, "" )
        print $ runWriter a
        putStrLn ""

    -- // ex_07
        putStrLn "// ex_07"
        print $ test
        print $ test1
        putStrLn ""

    -- // ex_08
        putStrLn "// ex_08"
        print $ test2
        putStrLn ""

        putStrLn "//"
        putStrLn ""


-- // ex_07

    test :: [ Char ] 
    test = execWriter $
        tell "Hello" >>= \ _ ->
        tell ", " >>= \ _ ->
        tell "World" >>= \ _ ->
        tell "!!" >>= \ _ ->
        return ( )

    test1 :: [ Char ] 
    test1 = execWriter $
        tellWr "Hello" `bindWr` \ _ ->
        tellWr ", " `bindWr` \ _ ->
        tellWr "World" `bindWr` \ _ ->
        tellWr "!!" `bindWr` \ _ ->
        returnWr ( )

    returnWr :: a -> Writer [ Char ] a
    returnWr x = writer ( x, "" )

    bindWr :: Writer [ Char ] a -> ( a -> Writer [ Char ] b ) -> Writer [ Char ] b
    bindWr m f =
        let ( x1, w1 ) = runWriter m
            ( x2, w2 ) = runWriter ( f x1 )
        in writer ( x2, w1 ++ w2 )

    tellWr :: [ Char ] -> Writer [ Char ] ( )
    tellWr xs = writer ( ( ), xs )

-- // ex_08
    test2 :: [ Char ] 
    test2 = execWriter $ do
        tell "Hello"
        tell ", "
        tell "World"
        tell "!!"
        return ( )


-- // fact

    factWt :: Int -> Writer [ String ] Int
    factWt 0 = do
        tell [ "fact 0 = 1" ]
        return 1
    factWt n | n > 0 = do
        let dbg = "fact " ++ show n ++ " = " ++
                  show n ++ " * fact " ++ show ( n - 1 )
        tell [ dbg ]
        n' <- factWt ( n - 1 )
        let ret = n * n'
        tell [ dbg ++ " = " ++ show n ++ " * " ++ show n' ++ " = " ++ show ret ]
        return ret

    factIO :: Int -> IO Int
    factIO 0 = do
        putStrLn $ "fact 0 = 1"
        return 1
    factIO n | n > 0 = do
        let dbg = "fact " ++ show n ++ " = " ++
                  show n ++ " * fact " ++ show ( n - 1 )
        putStrLn dbg
        n' <- factIO ( n - 1 )
        let ret = n * n'
        putStrLn $ dbg ++ " = " ++ show n ++ " * " ++ show n' ++ " = " ++ show ret
        return ret



