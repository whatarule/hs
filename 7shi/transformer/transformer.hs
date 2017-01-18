
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Writer
    import Control.Monad.List
    import Control.Monad.Identity
    import Control.Applicative
    import Data.Char

    main = do
        
        putStrLn "" 

        putStrLn "-- // Transformer" 
        putStrLn "" 

    --  print $ sum' [ 1..5 ]
        print =<< sum' [ 1..5 ]
        sum' [ 1..5 ]
        putStrLn "" 

    -- // identity
        putStrLn "// identity" 
    --  Identity a
        let a = return 1 :: Identity Int
        print $ runIdentity a
        putStrLn ""

    -- // id
        putStrLn "// id" 
    --  id :: a -> a    
        print $ 1
        print $ id 1
        print $ f_id id 1
        putStrLn "" 

    -- // StateT
        putStrLn "// StateT" 
    --  StateT s m a
    --  type State' s a = StateT s Identity a
    --  cf. IO a <=> ST RealWorld a
    --  s -> m ( a, s )
    --  s -> Identity ( a, s )
    --  runStateT :: ( Monad m ) => StateT s m a -> s -> m ( a, s )
    --  evalStateT :: ( Monad m ) => StateT s m a -> s -> m a
    --  execStateT :: ( Monad m ) => StateT s m a -> s -> m s 
    --  StateT :: ( s -> m ( a, s ) ) -> StateT s m a
        let a = return 1 :: StateT s Identity Int
        print $ evalState a ( )
        print $ runState a ( )
    --  let a = return 1 :: StateT s IO Int
    --  evalState a ( )
        putStrLn "" 
        
        print $ f_rSt ( ) 
        print $ runIdentity $ f_rStT ( )
        putStrLn "" 
        
        let st = return 1 :: StateT s Identity Int
        print $ runIdentity $ runStateT st ( )
        print $ runIdentity $ evalStateT st ( )
        print $ runIdentity $ execStateT st ( )
        putStrLn ""

        let st = StateT $ \ s -> Identity ( 1, s )
        print $ runIdentity $ runStateT st ( )
        putStrLn ""

    -- // ex_01
        putStrLn "// ex_01" 
        let st = return' 1
        print $ runState' st ( )
        putStrLn ""

    -- // IO
        putStrLn "// IO" 
        print =<< f_ioInt ( )
        putStrLn "" 
        let a = StateT f_ioInt :: StateT s IO Int
        print =<< runStateT a ( )
        print =<< f_ioIO 1
        putStrLn "" 
        let a = StateT f_ioIO :: StateT s IO ( )
        print =<< runStateT a 1
        putStrLn ""

    -- // lift
        putStrLn "// lift" 
    --  lift :: ( Monad m ) => m a -> t m a
        let a1 = StateT $ \ s -> do 
            a <- print "hello"
            return ( a, s ) :: IO ( ( ), Int ) 
        let a2 = lift $ print "hello" :: StateT s IO ( )
        print =<< runStateT a1 1
        print =<< runStateT a2 1
        putStrLn "" 
        let a = do
            v <- get
            lift $ print v :: StateT String IO ( )
        runStateT a "hello"
        putStrLn ""

    -- // ex_02
        putStrLn "// ex_02" 
        fact 5 >>= print
        putStrLn ""
        fact02 5 >>= print
        putStrLn ""

    -- // ex_03
        putStrLn "// ex_03" 
        fact03 5 >>= print
        putStrLn ""

    -- // Maybe
        putStrLn "// Maybe"
        print $ get3 "abcd"
        print $ get3 "1234"
    --  print $ get3 "a"
        putStrLn ""

        print $ get3St "abcd"
        print $ get3St "1234"
    --  print $ get3St "a"
        putStrLn ""

        print $ get3Mb "abcd"
        print $ get3Mb "1234"
        print $ get3Mb "a"
        putStrLn ""

        print $ get3StT "abcd"
        print $ get3StT "1234"
        print $ get3StT "a"
        putStrLn ""

    -- // ex_04
        putStrLn "// ex_04"
        print $ test "Aa0"
    --  print $ test "abc"
        putStrLn ""

        print $ testMb "Aa0"
        print $ testMb "abc"
        putStrLn ""

    -- // ex_05
        putStrLn "// ex_05"
        print $ testStT "Aa0"
        print $ testStT "abc"
        putStrLn ""

    -- // ex_06
        putStrLn "// ex_06"
        print $ testR 0
        print $ testW 0
        print $ testL 0
        putStrLn ""

        testRT 0
        testWT 0
        testLT 0
        putStrLn ""

    -- // lift $ lift
        putStrLn "// lift $ lift"
        test0 1
        putStrLn ""
        test1 1
        test2 1
        runStateT ( test2T 1 ) ( ) 
        putStrLn ""
        test1' 1
        test2T' 1
        runStateT ( test2T' 1 ) ( ) 
        putStrLn ""

    -- // ex_07 
        putStrLn "// ex_07"
        testRT' 0
        testWT' 0
        testLT' 0
        putStrLn ""

    -- // liftM
        putStrLn "// liftM"
    --  liftM :: ( Monad m ) => ( a1 -> r ) -> m a1 -> m r
    --  liftM2 :: ( Monad m ) => ( a1 -> a2 -> r ) -> m a1 -> m a2 -> m r
    --  print $ ( + 1 ) [ 1 ]
        print $ liftM ( + 1 ) [ 1 ]
        print $ liftM2 ( + ) [ 1 ] [ 1 ]
        putStrLn ""

    -- // Applicative
        putStrLn "// Applicative"
        print $ ( + 1 ) <$> [ 1 ]
        print $ ( + ) <$> [ 1 ] <*> [ 1 ]
        putStrLn ""
        print $ liftM ( + 1 ) [ 1 ] 
        print $ ( <$> ) ( + 1 ) [ 1 ] 
        putStrLn ""
        print $ ( + 1 ) `liftM` [ 1 ]
        print $ ( + 1 ) <$> [ 1 ]
        putStrLn ""
        print $ ( + ) `liftM` [ 1 ] <*> [ 1 ]
        print $ ( + ) <$> [ 1 ] <*> [ 1 ]
        putStrLn ""
        print $ liftM ( + ) [ 1 ] <*> [ 1 ]
        print $ ( <$> ) ( + ) [ 1 ] <*> [ 1 ]
        putStrLn ""

    -- // <$>
        putStrLn "// <$>"
        print $ ( + 1 ) <$> [ 1 ]
        print $ ( + 1 ) `appM'` [ 1 ]
        putStrLn ""

    -- // <*>
        putStrLn "// <*>"
        print $ ( + ) <$> [ 1 ] <*> [ 1 ]
        print $ ( + ) `appM'` [ 1 ] `app'` [ 1 ]
        putStrLn ""
        print $ return ( + ) <*> [ 1 ] <*> [ 1 ]
        putStrLn ""

    -- // first-class functions
        putStrLn "// first-class functions"
        print $ ( evalState f1 ( ) ) 1
        print $ ( evalState f2 ( ) ) 1
        print $ ( f3 !! 0 ) 1
        putStrLn ""


    -- // ex_08
        putStrLn "// ex_08"
        print [ ( x, y ) | x <- [ 0, 1 ], y <- [ 0, 2 ] ]
        print [ x + y | x <- [ 0, 1 ], y <- [ 0, 2 ] ]
        putStrLn ""
        print $ liftM ( , ) [ 0, 1 ] <*> [ 0, 2 ]
        print $ liftM ( + ) [ 0, 1 ] <*> [ 0, 2 ]
        putStrLn ""
        print $ liftM2 ( , ) [ 0, 1 ] [ 0, 2 ]
        print $ liftM2 ( + ) [ 0, 1 ] [ 0, 2 ]
        putStrLn ""

    -- // 
        putStrLn "// "
        putStrLn ""


-- // first-class functions
    f1 :: State s ( Int -> Int )
    f1 = return ( + 1 )

    f2 :: State s ( Int -> Int )
    f2 = state $ \ s -> ( ( + 1 ), s )

    f3 :: [ Int -> Int ]
    f3 = return ( + 1 )


-- // <*>
    app' :: ( Monad m ) => m ( a -> b ) -> m a -> m b
    mf `app'` m = do
        f <- mf
        a <- m
        return $ f a

-- // <$>
    appM' :: ( Monad m ) => ( a -> b ) -> m a -> m b
    f `appM'` m = do
        a <- m
        return $ f a


-- // ex_07
    testRT' :: Int -> IO ( )
    testRT' x = ( `runReaderT` x ) $ do
        a <- ask
        liftIO $ print $ a + 1 

    testWT' :: Int -> IO ( ( ), [ Char ] )
    testWT' x = runWriterT $ do
        tell ""
        liftIO $ print $ x + 1

    testLT' :: Int -> IO [ ( ) ]
    testLT' x = runListT $ do
        liftIO $ print $ do
            return $ x + 1 :: [ ] Int

-- // lift $ lift
    test0 :: Int -> IO ( ( ), Int )
    test0 x = ( `runStateT` x ) $ do
        modify ( + 1 )
        a <- get
        lift $ print a
        ( `runReaderT` a ) $ do
            b <- ask
            lift $ lift $ print $ b + 1

    test1 :: Int -> IO ( ( ), Int )
    test1 x = ( `runStateT` x ) $ do
        modify ( + 1 )
        get >>= test2T

    test2 :: Int -> IO ( )
    test2 x = ( `runReaderT` x ) $ do
        b <- ask
        lift $ print $ b + 1

    test2T :: ( MonadTrans t, Monad ( t IO ) ) => Int -> t IO ( )
    test2T x = ( `runReaderT` x ) $ do
        b <- ask
        lift $ lift $ print $ b + 1

    test1' :: Int -> IO ( ( ), Int )
    test1' x = ( `runStateT` x ) $ do
        modify ( + 1 )
        get >>= test2T'

    test2T' :: ( MonadIO m ) => Int -> m ( ) 
    test2T' x = ( `runReaderT` x ) $ do
        b <- ask
        liftIO $ print $ b + 1


-- // ex_06
    testR :: Int -> Int
    testR x = ( `runReader` x ) $ do
        a <- ask
        return $ a + 1

    testW :: Int -> Int
    testW x = fst $ runWriter $ do
        tell ""
        return $ x + 1

    testL :: Int -> [ Int ]
    testL x = do
        a <- [ x ]
        return $ a + 1

    testRT :: Int -> IO ( )
    testRT x = ( `runReaderT` x ) $ do
        a <- ask
        lift $ print $ a + 1 

    testWT :: Int -> IO ( ( ), [ Char ] )
    testWT x = runWriterT $ do
        tell ""
        lift $ print $ x + 1

    testLT :: Int -> IO [ ( ) ]
    testLT x = runListT $ do
        lift $ print $ do
            return $ x + 1 :: [ ] Int

-- // ex_05
    getchBlStT :: ( a -> Bool ) -> StateT [ a ] Maybe a
    getchBlStT f = StateT $ getchBl f where
        getchBl f ( x : xs )
            | f x = Just ( x, xs )
            | otherwise = Nothing

    testStT :: [ Char ] -> Maybe [ Char ] 
    testStT = evalStateT $ do
        ch1 <- getchBlStT isUpper
        ch2 <- getchBlStT isLower
        ch3 <- getchBlStT isDigit
        return [ ch1, ch2, ch3 ]

-- // ex_04
    getchBl :: ( a -> Bool ) -> [ a ] -> ( a, [ a ] )
    getchBl f ( x : xs )
        | f x = ( x, xs )

    test :: [ Char ] -> [ Char ] 
    test s0 =
        let ( ch1, s1 ) = getchBl isUpper s0
            ( ch2, s2 ) = getchBl isLower s1
            ( ch3, s3 ) = getchBl isDigit s2
        in [ ch1, ch2, ch3 ]

    getchBlMb :: ( a -> Bool ) -> [ a ] -> Maybe ( a, [ a ] )
    getchBlMb f ( x : xs )
        | f x = Just ( x, xs )
        | otherwise = Nothing

    testMb :: [ Char ] -> Maybe [ Char ] 
    testMb s0 = do
        ( ch1, s1 ) <- getchBlMb isUpper s0
        ( ch2, s2 ) <- getchBlMb isLower s1
        ( ch3, s3 ) <- getchBlMb isDigit s2
        return [ ch1, ch2, ch3 ]

-- // Maybe
    getch :: [ a ] -> ( a, [ a ] )
    getch ( x : xs ) = ( x, xs )

    get3 :: [ a ] -> [ a ]
    get3 xs0 =
        let ( x1, xs1 ) = getch xs0
            ( x2, xs2 ) = getch xs1
            ( x3, xs3 ) = getch xs2
        in [ x1, x2, x3 ]

    getchSt :: State [ a ] a 
    getchSt = state getch where
        getch ( x : xs ) = ( x, xs )

    get3St :: [ a ] -> [ a ]
    get3St = evalState $ do
        x1 <- getchSt
        x2 <- getchSt
        x3 <- getchSt
        return [ x1, x2, x3 ]

    getchMb :: [ a ] -> Maybe ( a, [ a ] )
    getchMb ( x : xs ) = Just ( x, xs )
    getchMb _ = Nothing

    get3Mb :: [ a ] -> Maybe [ a ]
    get3Mb xs0 = do
        ( x1, xs1 ) <- getchMb xs0
        ( x2, xs2 ) <- getchMb xs1
        ( x3, xs3 ) <- getchMb xs2
        return [ x1, x2, x3 ]

    getchStT :: StateT [ a ] Maybe a 
    getchStT = StateT getch where
        getch ( x : xs ) = Just ( x, xs )
        getch _ = Nothing

    get3StT :: [ a ] -> Maybe [ a ]
    get3StT = evalStateT $ do
        x1 <- getchStT
        x2 <- getchStT
        x3 <- getchStT
        return [ x1, x2, x3 ]


-- // ex_03
    fact03 :: Int -> IO Int 
    fact03 x = ( `execStateT` 1 ) $ do
        forM_ [ 1..x ] $ \ i -> do
            modify ( * i )
            v <- get
            lift $ putStrLn $ "*" ++ show i ++ " -> " ++ show v
    

-- // ex_02
    fact :: Int -> IO Int 
    fact x = ( `execStateT` 1 ) $
        forM_ [ 1..x ] $ \ i ->
            modify ( * i ) >>= \ _ ->
            get >>= \ v ->
            lift $ putStrLn $ "*" ++ show i ++ " -> " ++ show v
    
    fact02 :: Int -> IO Int 
    fact02 x = ( `execStateT` 1 ) $
        forM_ [ 1..x ] $ \ i ->
            modifyStT ( * i ) `bindStT` \ _ ->
            getStT `bindStT` \ v ->
            liftStT $ putStrLn $ "*" ++ show i ++ " -> " ++ show v
    
    bindStT :: ( Monad m ) => StateT s m a -> ( a -> StateT s m b ) -> StateT s m b
    bindStT m f = StateT $ \ s0 ->
        runStateT m s0 >>= \ ( x1, s1 ) ->
        runStateT ( f x1 ) s1 

    getStT :: ( Monad m ) => StateT s m s 
    getStT = StateT $ \ s -> return ( s, s )

    modifyStT :: ( Monad m ) => ( s -> s ) -> StateT s m ( )
    modifyStT f = StateT $ \ s -> return ( ( ), f s )

    liftStT :: ( Monad m ) => m a -> StateT s m a
    liftStT m = StateT $ \ s ->
        m >>= \ x ->
        return ( x, s )



-- // IO
    f_ioInt :: s -> IO ( Int, s )
    f_ioInt s = return ( 1, s )

    f_ioIO :: s -> IO ( ( ), s )
    f_ioIO s = do
        a <- print "hello"
        return ( a, s )

-- // runStateT
    st = return 1 :: State s Int
    
    f_rSt :: s -> ( Int, s )
    f_rSt = runState st

    f_rStT :: s -> Identity ( Int, s )
    f_rStT = runStateT st

-- // ex_01
    return' :: a -> StateT s Identity a
    return' x = StateT $ \ st -> Identity ( x, st )

    runState' :: StateT s Identity a -> s -> ( a, s ) 
    runState' st = runIdentity . runStateT st


-- // id
    f_id :: ( a -> b ) -> a -> b
    --  f_id g x = g x
    f_id g = g
    --  f_id =
    --  f_id = id


    sum' :: [ Int ] -> IO ( Int )
    sum' xs =
    --  ( `execState` 0 ) $ do
        ( `execStateT` 0 ) $ do
            forM_ xs $ \ i -> do
                modify ( + i )
                v <- get
            --  putStrLn $ "+" ++ show i ++ " -> " ++ show v
                lift $ putStrLn $ "+" ++ show i ++ " -> " ++ show v



