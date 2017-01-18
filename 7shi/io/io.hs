
{-# LANGUAGE UnboxedTuples, MagicHash #-}

    import GHC.Base
    import GHC.IO
    import Control.Monad
    import Control.Monad.ST
    import Control.Monad.State
    import System.Random

    unIO' :: IO a -> ( State# RealWorld ) -> (# State# RealWorld, a #)
    unIO' ( IO a ) = a

    unsafeDupablePerformIO' :: IO a -> a
    unsafeDupablePerformIO' ( IO m ) = lazy ( case m realWorld# of (#  _, r #) -> r )

    unsafePerformIO' :: IO a -> a
    unsafePerformIO' m = unsafeDupablePerformIO ( noDuplicate >> m )

    returnIO :: a -> IO a
    returnIO x = IO $ \ s -> (# s, x #)

    bindIO :: IO a -> ( a -> IO b ) -> IO b
    bindIO ( IO m ) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO ( k a ) new_s

    main = do
        
        print $ "-----"
    
    -- // action
        r <- dice
        print $ r
        print $ "-----"

    -- // return
        let a = return 1
        print =<< a
        print =<< a
        print $ "-----"
        print =<< dice
        print =<< dice
        print $ "-----"
        let hello = putStr "hello"
        print =<< hello
        print =<< hello
        print $ "-----"
        
    -- // unIO
        let hello1 = unIO $ putStr "hello"
        let hello2 = IO hello1
        print =<< hello2
        print $ "-----"
        let m = return 1
        print =<< m
        print $ "-----"
        let f = unIO m 
        let m1 = IO f 
        print =<< m1
        print $ "-----"
        v <- m
        let m2 = return v
        print =<< m2
        print $ "-----"

    -- // UnboxedTuples
        let (# a, b #) = addsub 1 2
        print ( a, b )
        print $ "-----"
    
    -- // return
        let a = IO $ \ s -> (# s, 1 #)
        a
        print =<< a
        print $ "-----"

    -- // main
        let main1 = IO $ \ s -> (# s, ( ) #)
        main1
        print =<< main1
        print $ "-----"
        IO $ \ s -> (# s, ( ) #)
        print =<< ( IO $ \ s -> (# s, ( ) #) )
        print $ "-----"
        let main1 = IO $ \ s -> (# s, "hello" #)
        main1
        print =<< main1
        print $ "-----"
        let hello = unIO ( print "hello" )
        IO hello
        print =<< IO hello
        print $ "-----"
        let (# _, b #) = unIO ( print "hello" ) realWorld#
        print ( ( ), b )
        print $ "-----"
        IO $ \ s ->
            let (# s1, r #) = unIO ( print "hello" ) s
            in (# s1, r #)
        print $ "-----"
        main2
        print =<< main2
        print $ "-----"

    -- // State# RealWorld
        IO main'
        print $ "-----"

-- //
    -- //  realWold#
        let f = unIO $ print "a"       
        IO f
        print $ "-----"
    --  f realWorld#
        let a = f realWorld#
        IO $ \ s -> a 
        print $ "-----"
    --  let (# a, b #) = f realWorld#
        let world = realWorld#
        let (# _, b #) = f realWorld#
        print $ b
        print $ "-----"
        let dice1 = unIO $ dice
        print $ let (# _, b #) = dice1 realWorld# in b
        print $ let (# _, b #) = dice1 realWorld# in b
        print $ let (# _, b #) = dice1 realWorld# in b
        print $ "-----"
        IO $ \ world ->
            unIO ( print "hello" ) world
        print $ "-----"
        IO $ \ world ->
            let (# world1, _ #) = unIO ( print "hello" ) world
                (# world2, _ #) = unIO ( print "world" ) world1
            in  (# world2, ( ) #)
        print $ "-----"
        IO $ \ world ->
            let _ = unIO ( print "hello" ) world
                _ = unIO ( print "world" ) world
            in  (# world, ( ) #)
        print $ "-----"
        IO $ \ world ->
            let _ = unIO ( print "world" ) world
                _ = unIO ( print "hello" ) world
            in  (# world, ( ) #)
        print $ "-----"
        IO $ \ _ ->
            let _ = unIO ( print "hello" ) realWorld#
                _ = unIO ( print "world" ) realWorld#
            in  (# realWorld#, ( ) #)
        print $ "-----"

    -- // memoize
        IO $ \ _ ->
            let f = unIO ( print "hello" )
                _ = f realWorld#
                _ = f realWorld#
                _ = f realWorld#
            in  (# realWorld#, ( ) #)
        print $ "-----"
        IO $ \ world ->
            let f = unIO ( print "hello" )
                (# world1, _ #) = f world
                (# world2, _ #) = f world1
                (# world3, _ #) = f world2
            in  (# world3, ( ) #)
        print $ "-----"
-- //

    -- // loop
        let loop i | i <= 3 = do
                print i
                loop $ i + 1
            loop _ = return ( )
        loop 1
        print $ "-----"

    -- // do
        test1 >> test2 >> test3
        print $ "-----"

    -- // ex_01
        print =<< ( shuffle [ ] :: IO [ Int ] )
        print =<< shuffle [ 1..9 ]
        print $ "-----"
        print =<< ( shuffle' [ ] :: IO [ Int ] )
        print =<< shuffle' [ 1..9 ]
        print $ "-----"

    -- // bind
        let test1 = ( return "1" >>= putStr ) >>= print
        let test2 = return "2" >>= putStr >>= print
        test1 >> test2
        print $ "-----"

    -- // ex_02
        return "hello" >>= putStr >>= print
        return' "hello" `bind'` putStr `bind'` print
        print $ "-----"


-- // ex_02
    return' :: a -> IO a
    return' x = IO $ \ w -> (# w, x #)

    bind' :: IO a -> ( a -> IO b ) -> IO b
    bind' io f = IO $ \ w0 -> 
        let (# w1, x1 #) = unIO io w0
            (# w2, x2 #) = unIO ( f x1 ) w1
        in (# w2, x2 #)

-- // ex_01
    shuffle :: [ a ] -> IO [ a ]
    shuffle [ ] = return [ ]
    shuffle xs = do
        n <- getStdRandom $ randomR ( 0, length xs - 1 ) :: IO Int
        xs' <- shuffle $ take n xs ++ drop ( n + 1 ) xs
        return $ ( xs !! n ) : xs'

    shuffle' :: [ a ] -> IO [ a ]
    shuffle' [ ] = IO $ \ w -> (# w, [ ] #)
    shuffle' xs = IO $ \ w0 -> 
        let (# w1, n #) = unIO ( getStdRandom $ randomR ( 0, length xs - 1 ) :: IO Int ) w0
            (# w2, xs' #) = unIO ( shuffle' $ take n xs ++ drop ( n + 1 ) xs ) w1
        in (# w2, ( xs !! n ) : xs' #)


-- // do
    test1 = do
        a <- return 1
        print a
    test2 =
        return 2 >>= \ a ->
        print a
    test3 = IO $ \ s ->
        let (# s1, a #) = unIO ( return 3 ) s
            (# s2, r #) = unIO ( print a ) s1
        in (# s2, r #)


-- // Stete# RealWorld
    main' :: State# RealWorld -> (# State# RealWorld, ( ) #)
    main' s0 =
        let (# s1, _ #) = unIO ( print "hello" ) s0
            (# s2, _ #) = unIO ( print "world" ) s1
            (# s3, r #) = unIO ( print "!!" ) s2
        in  (# s3, r #)


-- // main
    main2 :: IO ( )
    main2 = IO $ \ s ->
        let (# s1, r #) = unIO ( print "hello" ) s
        in (# s1, r #)


-- // UnboxedTuples
    addsub :: ( Num a ) => a -> a -> (# a, a #)
    addsub x y = (# x + y, x - y #)


-- // action
    dice :: IO Int
    dice = getStdRandom $ randomR ( 1, 6 )



