
    import Control.Monad.State
    import Control.Monad.Reader

    main = do

        putStrLn $ ""
    
    -- // Reader
        putStrLn $ "-- // Reader"
    --  Reader r a
    --  cf. State s a
        putStrLn $ ""

    -- // ask
        putStrLn $ "// ask"
    --  ask :: Reader r r
    --  cf. get :: State s s

    -- // runReader
        putStrLn $ "// runReader"
    --  runReader :: Reader r a -> r -> a
    --  runState :: State s a -> s -> ( a, s )
        print $ ( `runState` 1 ) $ do
            a <- get
            return $ a + 1
        
        print $ ( `runReader` 1 ) $ do
            a <- ask
            return $ a + 1
        putStrLn $ ""
        
    -- // reader
        putStrLn $ "// reader"
    --  reader :: ( r -> a ) -> Reader r a
    --  cf. state :: ( s -> ( a, s ) ) -> State s a
        let a = reader $ \ _ -> 1
        print $ runReader a ( )
        putStrLn $ ""
        
    -- // local
        putStrLn $ "// local"
    --  local :: ( r -> r ) -> Reader r a -> Reader r a
    --  modify :: ( s -> s ) -> State s ( )
        print $ ( `runReader` 1 ) $ do
            a <- ask
            b <- local ( + 1 ) $ do
                b' <- ask
                return b'
            c <- ask
            return ( a, b, c )
        putStrLn $ ""
        
    -- ex_05
        putStrLn $ "// ex_05"
        print $ test 1
        print $ test1 1
        putStrLn $ ""
        
    -- ex_06
        putStrLn $ "// ex_06"
        print $ test2 1
        putStrLn $ ""
        
        putStrLn $ "//"
        putStrLn $ ""


-- // ex_06
    test2 :: Int -> ( Int, Int, Int )
    test2 x = ( `runReader` x ) $ do
        a <- ask
        b <- local ( + 1 ) $ do
            b' <- ask
            return b'
        c <- ask
        return ( a, b, c )

-- ex_05
    test :: Int -> ( Int, Int, Int )
    test x = ( `runReader` x ) $
        ask >>= \ a ->
        ( local ( + 1 ) $
            ask >>= \ b' ->
            return b' ) >>= \ b ->
        ask >>= \ c ->
        return ( a, b, c )

    test1 :: Int -> ( Int, Int, Int )
    test1 x = ( `runReader` x ) $
        ask `bindRd` \ a ->
        ( localRd ( + 1 ) $
            ask `bindRd` \ b' ->
            returnRd b' ) `bindRd` \ b ->
        ask `bindRd` \ c ->
        returnRd ( a, b, c )
    
    returnRd :: a -> Reader r a
    returnRd x = reader $ \ r -> x

    bindRd :: Reader r a -> ( a -> Reader r b ) -> Reader r b
    bindRd m f = reader $ \ r ->
        let x = runReader m r
        in runReader ( f x ) r 

    askRd :: Reader r r
    askRd = reader $ \ r -> r

    localRd :: ( r -> r ) -> Reader r a -> Reader r a
    localRd f m = reader $ \ r -> runReader m ( f r )


