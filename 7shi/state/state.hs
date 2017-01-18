
{-# LANGUAGE MagicHash, UnboxedTuples #-}

    import Control.Monad
    import Control.Monad.ST
    import Control.Monad.State
    import Data.IORef
    import Data.STRef
    import Data.Array.ST
    import GHC.Base
    import GHC.IO
    import GHC.ST

    unST :: ST s a -> ( State# s -> (# State# s, a #) )
    unST ( ST f ) = f


    main = do
        print $ "-------" 
    
    -- // IORef
        a <- newIORef 1
        writeIORef a 2
        print =<< readIORef a
        print $ "-------" 
        print =<< sum1 [  1..100 ]
        print $ "-------" 

    -- // STRef
    --  runST :: ( forall s. ST s a ) -> a
        print $ sum2 [ 1..100 ]
        print $ "-------" 
        let a = do
            b <- newSTRef 1
            modifySTRef b ( + 1 )
            readSTRef b
        print $ runST a
        print $ "-------" 
        let a = return 1 :: ST s Int
        print $ runST a
        print $ "-------" 
    {-
        let a = return 1 :: ST Int Int
        print $ runST a
        print $ "-------"    
    -}

    -- // unST
    --  ST s a
    --  ST RealWorld a <=> IO a
    --  State# s -> (# State# s, a #)
    --  State# RealWorld -> (# State# RealWorld, a #)
        IO $ \ s0 ->
            let f1 = unST $ return 2
                f2 = unIO $ print r1
                (# s1, r1 #) = f1 s0
                (# s2, r2 #) = f2 s1
            in (# s2, r2 #)
        print $ "-------" 
    --  realWorld# :: State# RealWorld
        let f = unST $ return 1
            (# _, a #) = f realWorld#
        print $ a
        print $ "-------" 
        
    -- // stToIO
    --  stToIO :: ST RealWorld a -> IO a
        let a = return 2 :: ST s Int
        print =<< stToIO a
        print $ "-------" 
    --  ioToST :: IO a -> ST RealWorld a
        let a = return 1 :: IO Int
        let (# _, b #) = unST ( ioToST a ) realWorld#
        print $ b
        print $ "-------"
        
    -- // STUArray
        let arr = runST $ do
            a <- newArray ( 0, 5 ) 0 :: ST s ( STUArray s Int Int )
            forM_ [ 0..5 ] $ \ i ->
                writeArray a i i
            getElems a
        print $ arr
        print $ "-------"
        
    -- // ex_01
    --  Brainf*ck???

    -- // ex_02
        print $ runST $
            return 1 >>= newSTRef >>= \ a ->
            modifySTRef a ( + 1 ) >>= \ _ ->
            readSTRef a >>= return
        print $ "-------"
        print $ runST $
            returnST 1 `bindST` newSTRef `bindST` \ a ->
            modifySTRef a ( + 1 ) `bindST` \ _ ->
            readSTRef a `bindST` returnST
        print $ "-------"

    -- // State
    --  State s a
    --  runState :: State s a -> s -> ( a, s )
    --  evalState :: State s a -> s -> a
    --  execState :: State s a -> s -> s
        let a = return 1 :: State s Int
        print $ runState a ( )
        print $ evalState a ( )
        print $ execState a ( )
        print $ "-------"
        let f = runState $ return "State" :: ( ) -> ( String, ( ) )
        let ( a, _ ) = f ( )
        print $ a
        print $ "-------"
        let f = unIO $ return "IO" :: State# RealWorld -> (# State# RealWorld, String #)
        let (# _, a #) = f realWorld#
        print $ a
        print $ "-------"
    --  state :: ( s -> ( a, s ) ) -> State s a
        let m1 = state $ \ s -> ( 1, s )
            m2 = return 1
        print $ runState m1 ( )
        print $ runState m2 ( )
        print $ "-------"
        
    -- // State action
    --  get :: State s s
    --  put :: s -> State s ( )
    --  modify :: ( s -> s ) -> State s ( )
        let test = do
            a <- get
            put $ a + 1
            modify ( * 2 )
            return a
        print $ runState test 5
        print $ "-------"
        let test = do
            a <- get
            put $ a + 1
            b <- get
            put $ b * 2
            return b
        print $ runState test 5
        print $ "-------"
        let test = do
            modify ( + 1 )
            modify ( * 2 )
        print $ runState test 5
        print $ "-------"
        print $ sum3 [ 1..100 ]
        print $ "-------"
        print $ evalState getch1 "abcd"
        print $ execState getch1 "abcd"
        print $ evalState getThree1 "abcd"
        print $ execState getThree1 "abcd"
        print $ "-------"
        print $ fst $ getch2 "abcd"
        print $ snd $ getch2 "abcd"
        print $ fst $ getThree2 "abcd"
        print $ snd $ getThree2 "abcd"
        print $ "-------"
        print $ evalState getch3 "abcd"
        print $ execState getch3 "abcd"
        print $ evalState getThree3 "abcd"
        print $ execState getThree3 "abcd"
        print $ "-------"
        print $ evalState getch4 "abcd"
        print $ execState getch4 "abcd"
        print $ "-------"
        
    -- // ex_03
        print $ fib 10
        print $ fib1 10
        print $ "-------"
        
    -- // ex_04
        print $ fib2 10
        print $ "-------"
        
        print $ "-------"
        print $ "-------"



-- // ex_04
    fib2 :: Int -> Int
    fib2 x = ( `evalState` ( 0, 1 ) ) $ do
        replicateM_ ( x - 1 ) $ do
            ( a, b ) <- get
            put ( b, a + b )
        v <- get
        return $ snd v

-- // ex_03
    fib :: Int -> Int
    fib x = ( `evalState` ( 0, 1 ) ) $
        ( replicateM_ ( x - 1 ) $
            get >>= \ ( a, b ) ->
            put ( b, a + b ) ) >>= \ _ ->
        get >>= \ v ->
        return $ snd v

    fib1 :: Int -> Int
    fib1 x = ( `evalState` ( 0, 1 ) ) $
        ( replicateM_ ( x - 1 ) $
            getSt `bindSt` \ ( a, b ) ->
            putSt ( b, a + b ) ) `bindSt` \ _ ->
        getSt `bindSt` \ v ->
        returnSt $ snd v
    
    returnSt :: a -> State s a
    returnSt x = state $ \ s -> ( x, s )
    
    bindSt :: State s a -> ( a -> State s b ) -> State s b
    bindSt m f = state $ \ s0 ->
        let ( x1, s1 ) = runState m s0
            ( x2, s2 ) = runState ( f x1 ) s1
        in ( x2, s2 )

    getSt :: State s s
    getSt = state $ \ s -> ( s, s )

    putSt :: s -> State s ( )
    putSt s' = state $ \ s -> ( ( ), s' )


-- // State action
--  execState a b <=> a `execState` b <=> ( `execState` b ) a
    sum3 :: [ Int ] -> Int
    sum3 xs = ( `execState` 0 ) $ do
        forM_ xs $ \ i ->
            modify ( + i )

    getch1 :: State [ a ] a 
    getch1 = do
        x : xs <- get
        put xs
        return x
    getThree1 :: State [ a ] [ a ]
    getThree1 = do
        x1 <- getch1
        x2 <- getch1
        x3 <- getch1
        return [ x1, x2, x3 ]

    getch2 :: [ a ] -> ( a, [ a ] )
    getch2 ( x : xs ) = ( x, xs )
    getThree2 :: [ a ] -> ( [ a ], [ a ] )
    getThree2 s0 =
        let ( x1, s1 ) = getch2 s0
            ( x2, s2 ) = getch2 s1
            ( x3, s3 ) = getch2 s2
        in ( [ x1, x2, x3 ], s3 )

    getch3 :: State [ a ] a
    getch3 = state getch3 where
        getch3 ( x : xs ) = ( x, xs )
    getThree3 :: State [ a ] [ a ]
    getThree3 = do
        x1 <- getch3
        x2 <- getch3
        x3 <- getch3
        return [ x1, x2, x3 ]

    getch4 :: State [ a ] a
    getch4 = state $ \ ( x : xs ) -> ( x, xs )

-- // ex_02
    returnST :: a -> ST s a
    returnST x = ST $ \ s -> (# s, x #)

    bindST :: ST s a -> ( a -> ST s b ) -> ST s b
    bindST m f = ST $ \ s0 ->
        let (# s1, x1 #) = unST m s0
            (# s2, x2 #) = unST ( f x1 ) s1
        in (# s2, x2 #)

-- // IORef
    sum1 :: [ Int ] -> IO Int
    sum1 xs = do
        v <- newIORef 0
        forM_ xs $ \ i ->
            modifyIORef v ( + i )
        readIORef v

-- // ST
    sum2 :: [ Int ] -> Int
    sum2 xs = runST $ do
        v <- newSTRef 0
        forM_ xs $ \ i ->
            modifySTRef v ( + i )
        readSTRef v


