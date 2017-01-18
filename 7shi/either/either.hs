
    import Data.Char
    import Data.Either
    import Control.Monad
    import Control.Monad.State
    import Control.Exception

    main = do
    
    -- // Either
        putStrLn $ "// Either"
    --  Either a b
        print $ ( return 1 :: Either ( ) Int )
        putStrLn $ ""

    -- // either
        putStrLn $ "// either"
    --  either :: ( a -> c ) -> ( b -> c ) -> Either a b -> c
        let test0 = either ( + 1 ) ( * 2 )
        print $ test0 $ Left 4
        print $ test0 $ Right 4
        putStrLn $ ""
        print $ either id id $ Left 5
        print $ either id id $ Right 8
        putStrLn $ ""
        let test1 = do
            a <- Right 1
            Left ( )
            return a
        let test2 = do
            a <- Just 1
            Nothing
            return a
        print $ test1
        print $ test2
        putStrLn $ ""
        print $ test3 4
        print $ test3 8
        putStrLn $ ""
        let test4 x = either id id $ do
            when ( x < 5 ) $ Left $ x + 1
            return $ x * 2
        print $ test4 4
        print $ test4 8
        putStrLn $ ""

    -- // ex_01
        putStrLn $ "// ex_01"
        print $ testMb "Aa0"
        print $ testMb "A"
        print $ testMb "aa0"
        print $ testMb "AA0"
        print $ testMb "Aaa"
        putStrLn $ ""
        print $ testEi "Aa0"
        print $ testEi "A"
        print $ testEi "aa0"
        print $ testEi "AA0"
        print $ testEi "Aaa"
        putStrLn $ ""

    -- // ex_02
        putStrLn $ "// ex_02"
        print $ fizzbuzz 3
        print $ fizzbuzz 5
        print $ fizzbuzz 15
        print $ fizzbuzz 17
        putStrLn $ ""
        mapM_ ( putStrLn . either id id . fizzbuzz ) [ 1..15 ]
        putStrLn $ ""
        forM_ [ 1..15 ] $ putStrLn . fizzbuzz'
        putStrLn $ ""

    -- // Exception
        putStrLn $ "// Exception"
    --  catch :: Exception e => IO a -> ( e -> IO a ) -> IO a
    --  catches :: IO a -> [ Handler a ] -> IO a
        let foo 1 = "1"
    --  print $ foo 0
        print $ foo 1
        print $ "end"
        putStrLn $ ""
        catch ( do
            print $ foo 0
            print $ foo 1
            ) $ \ ( SomeException e ) ->
                print e
        print $ "end"
        putStrLn $ ""
        do
            print $ foo 0
            print $ foo 1
            `catch` \ ( SomeException e ) ->
                print e
        print $ "end"
        putStrLn $ ""
    {-
        do
            print $ 1 `div` 0
            `catch` \ ( PatternMatchFail _ ) ->
                print "PatternMatchFail"
    -}
        print $ "end"
        putStrLn $ ""
        do
            print $ 1 `div` 0
            `catches`
                [ Handler $ \ ( PatternMatchFail _ ) ->
                    print "PatterMatchFail"
                , Handler $ \ ( DivideByZero ) ->
                    print "DivideByZero"
                , Handler $ \ ( SomeException _ ) ->
                    print "SomeException"
                ]
        print $ "end"
        putStrLn $ ""
        do
            print $ foo 0
            `catches`
                [ Handler $ \ ( PatternMatchFail e ) -> do
                    putStrLn "PatterMatchFail:"
                    putStrLn $ show e
                , Handler $ \ ( DivideByZero ) -> do
                    putStrLn "DivideByZero"
                , Handler $ \ ( SomeException e ) -> do
                    putStrLn "SomeException:"
                    putStrLn $ show e
                ]
        print $ "end"
        putStrLn $ ""
    
    -- // ex_03
        putStrLn $ "// ex_03"
    {-
        forM_ [ "1", "a", "3" ] $ \ s ->
            print ( read s :: Int )
    -}
        forM_ [ "1", "a", "3" ] $ \ s -> do
            print ( read s :: Int )
            `catch` \ ( SomeException _ ) ->
                print $ s
        putStrLn $ ""

    -- // evaluate
        putStrLn $ "// evaluate"
    --  evaluate :: a -> IO a
    --  print =<< test5rt 
        print =<< test5ev 
        putStrLn $ ""
        print $ fooEi 0
        putStrLn $ ""

    -- // ex_04
        putStrLn $ "// ex_04"
    {-
        do
            forM_ [ 0..3 ] $ \ i -> do
                a <- try $ return $ 6 `div` i
                print $ ( a :: Either SomeException Int )
    -}
        do
            forM_ [ 0..3 ] $ \ i -> do
                a <- try $ evaluate $ 6 `div` i
                print $ ( a :: Either SomeException Int )
        putStrLn $ ""

    -- // getch
        putStrLn $ "// getch"
        print $ getThree "abcd"
        print $ getThree "1234"
    --  print $ getThree "a"
        putStrLn $ ""
        print $ getThreeSt "abcd"
        print $ getThreeSt "1234"
    --  print $ getThreeSt "a"
        putStrLn $ ""
        print $ getThreeEi "abcd"
        print $ getThreeEi "1234"
        print $ getThreeEi "a"
        putStrLn $ ""

    -- // EitherT
        putStrLn $ "// EitherT"
        print $ getThreeStT "abcd"
        print $ getThreeStT "1234"
        print $ getThreeStT "a"
        putStrLn $ ""

    -- // ex_05
        putStrLn $ "// ex_05"
        print $ testStT "Aa0"
        print $ testStT "A"
        print $ testStT "aa0"
        print $ testStT "AA0"
        print $ testStT "Aaa"
        putStrLn $ ""

    -- // 
        putStrLn $ "// "
        putStrLn $ ""

-- // 

-- // ex_05
    getchStT :: ( Char -> Bool ) -> String -> StateT [ Char ] ( Either String ) Char 
    getchStT f str = StateT getch
        where
            getch ( x : xs )
                | f x = Right ( x, xs )
                | otherwise = Left $ "not " ++ str ++ " : " ++ show x
            getch _ = Left $ "too short"

    testStT :: [ Char ] -> Either String [ Char ]
    testStT = evalStateT $ do
        ch0 <- getchStT isUpper "upper"
        ch1 <- getchStT isLower "lower"
        ch2 <- getchStT isDigit "digit"
        return [ ch0, ch1, ch2 ]


-- // EitherT
    getOneStT :: StateT [ a ] ( Either String ) a
    getOneStT = StateT getOne
        where
            getOne ( x : xs ) = Right ( x, xs )
            getOne _ = Left "too short"

    getThreeStT :: [ a ] -> Either String [ a ]
    getThreeStT = evalStateT $ do
        x1 <- getOneStT
        x2 <- getOneStT
        x3 <- getOneStT
        return [ x1, x2, x3 ]

-- // getch
    getOne :: [ a ] -> ( a, [ a ] )
    getOne ( x : xs ) = ( x, xs )

    getThree :: [ a ] -> [ a ]
    getThree xs0 =
        let ( x1, xs1 ) = getOne xs0
            ( x2, xs2 ) = getOne xs1
            ( x3, xs3 ) = getOne xs2
        in [ x1, x2, x3 ]

    getOneSt :: State [ a ] a
    getOneSt = state getOne
        where getOne ( x : xs ) = ( x, xs )

    getThreeSt :: [ a ] -> [ a ]
    getThreeSt = evalState $ do
        x1 <- getOneSt
        x2 <- getOneSt
        x3 <- getOneSt
        return [ x1, x2, x3 ]

    getOneEi :: [ a ] -> Either String ( a, [ a ] )
    getOneEi ( x : xs ) = Right ( x, xs )
    getOneEi _ = Left "too short"

    getThreeEi :: [ a ] -> Either String [ a ]
    getThreeEi xs0 = do
        ( x1, xs1 ) <- getOneEi xs0
        ( x2, xs2 ) <- getOneEi xs1
        ( x3, xs3 ) <- getOneEi xs2
        return [ x1, x2, x3 ]

-- // evaluate
    foo :: Int -> String
    foo 1 = "1"
    test5rt :: IO String
    test5rt = do
        return $ foo 0
        `catch` \ ( SomeException _ ) ->
            return "???"

    test5ev :: IO String
    test5ev = do
        evaluate $ foo 0
        `catch` \ ( SomeException _ ) ->
            return "???"

    fooEi :: Int -> Either String String
    fooEi 1 = Right "1"
    fooEi _ = Left "???"

-- // ex_02
    fizzbuzz :: Int -> Either String String
    fizzbuzz n
        | mod3 n && mod5 n = return "Fizzbuzz"
        | mod3 n = return "Fizz"
        | mod5 n = return "Buzz"
        | otherwise = Left $ show n
        where
            mod3 n = n `mod` 3 == 0
            mod5 n = n `mod` 5 == 0

    fizzbuzz' :: Int -> String
    fizzbuzz' n = either id id $ case n of 
        _ | mod3 n && mod5 n -> Left "Fizzbuzz"
        _ | mod3 n -> Left "Fizz"
        _ | mod5 n -> Left "Buzz"
        _ | otherwise -> return $ show n
        where
            mod3 n = n `mod` 3 == 0
            mod5 n = n `mod` 5 == 0


-- // ex_01
    getchMb :: [ a ] -> Int -> Maybe a
    getchMb s n
        | n < length s = Just $ s !! n
        | otherwise = Nothing

    testMb :: [ Char ] -> Maybe [ Char ]
    testMb s = do
        ch0 <- getchMb s 0
        ch1 <- getchMb s 1
        ch2 <- getchMb s 2
        unless ( isUpper ch0 ) Nothing
        unless ( isLower ch1 ) Nothing
        unless ( isDigit ch2 ) Nothing
        return [ ch0, ch1, ch2 ]

    getchEi :: [ a ] -> Int -> Either String a
    getchEi s n
        | n < length s = Right $ s !! n
    --  | otherwise = Left $ show n ++ " is out of the index."
        | otherwise = Left $ "out of range : " ++ show n

    testEi :: [ Char ] -> Either String [ Char ]
    testEi s = do
        ch0 <- getchEi s 0
        ch1 <- getchEi s 1
        ch2 <- getchEi s 2
    --  unless ( isUpper ch0 ) $ Left ( show ch0 ++ " is not uppercase." )
    --  unless ( isLower ch1 ) $ Left ( show ch1 ++ " is not lowecase." )
    --  unless ( isDigit ch2 ) $ Left ( show ch2 ++ " is not a digit." )
        unless ( isUpper ch0 ) $ Left ( "not uppercase : " ++ show ch0 )
        unless ( isLower ch1 ) $ Left ( "not lowecase : " ++ show ch1 )
        unless ( isDigit ch2 ) $ Left ( "not digit : " ++ show ch2 )
        return [ ch0, ch1, ch2 ]

-- // Either
-- // either
    test3 :: Int -> Either String Int
    test3 x = do
        when ( x < 5 ) $ Left $ show x ++ " < 5"
        return $ x * 2



