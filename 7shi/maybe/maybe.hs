
    import Control.Applicative
    import Data.Maybe

    import Control.Monad
    import Data.Char

    import Debug.Trace

    main = do

        putStrLn ""
        
    -- // Maybe
        putStrLn "// Maybe"
    --  Maybe a
        let a = return 1 :: Maybe Int
            b = Just 1
        print ( a, b )
        putStrLn ""

    -- // fact
        putStrLn "// fact"
        print $ fact 5 
        print $ fact ( -1 )
        putStrLn ""

    -- // ex_01 
        putStrLn "// ex_01"
        print $ fib 6
        print $ fibMb 0
        print $ fibMb 1
        print $ fibMb 6
        print $ fibMb ( -1 )
        putStrLn ""

    -- // Nothing
        putStrLn "// Nothing"
        print $ do
            a <- Just 1
            b <- Just 2
            return $ a * b
        print $ do
            a <- Just 1
            b <- Nothing
            return $ a * b
        putStrLn ""
        print $ do
            a <- [ 1 ]
            b <- [ 2 ]
            return $ a * b
        print $ do
            a <- [ 1 ]
            b <- [ ]
            return $ a * b
        putStrLn ""
        let a = Just 1
            b = Nothing
            ( Just a' ) = a
            ( Just b' ) = b
        print $ a'
    --  print $ b'
        putStrLn ""
        print $ test ( Just 1 )
        print $ test Nothing
        putStrLn ""

    -- // ex_02
        putStrLn "// ex_02"
        print $ Just 1 `bindMb` \ a -> Just 2 `bindMb` \ b -> Just $ a * b
        print $ Just 1 `bindMb` \ a -> Nothing `bindMb` \ b -> Just $ a * b
        putStrLn ""

    -- // mapMaybe
        putStrLn "// mapMaybe"
    --  mapMaybe :: ( a -> Maybe b ) -> [ a ] -> [ b ]
        print $ facts 3
        print $ facts 2
        print $ facts 1
        print $ facts 0
        putStrLn ""
        print $ factsLs 3
        print $ factsLs 2
        print $ factsLs 1
        print $ factsLs 0
        putStrLn ""

        putStrLn ""

    -- // ex_03 
        putStrLn "// ex_03"
        print $ mapMaybeL fact [ 2, 1, 0 ]
        print $ mapMaybeL fact [ 1, 0, -1 ]
        putStrLn ""
        print $ mapMaybeR fact [ 2, 1, 0 ]
        print $ mapMaybeR fact [ 1, 0, -1 ]
        putStrLn ""
        print $ mapMaybeF fact [ 2, 1, 0 ]
        print $ mapMaybeF fact [ 1, 0, -1 ]
        putStrLn ""

    -- // guard 
        putStrLn "// guard"
    --  guard :: MonadPlus m => Bool -> m ( )
        print $ do
            Nothing
            return 1
        print $ do
            [ ]
            return 1
        putStrLn ""
        print $ testStr' "Aa0"
        print $ testStr' "Aa"
        print $ testStr' "AA0"
        print $ testStr' "aa0"
        print $ testStr' "Aaa"
        putStrLn ""
        print $ testStr "Aa0"
        print $ testStr "Aa"
        print $ testStr "AA0"
        print $ testStr "aa0"
        print $ testStr "Aaa"
        putStrLn ""

    -- // ex_05
        putStrLn "// ex_05"
        print $ numUpper' 3 2 "123AB"
        print $ numUpper' 3 2 "123ABC"
        print $ numUpper' 3 2 "12ABC"
        putStrLn ""
        print $ numUpper 3 2 "123AB"
        print $ numUpper 3 2 "123ABC"
        print $ numUpper 3 2 "12ABC"
        putStrLn ""

    -- // Alternative
        putStrLn "// Alternative"
        trace "aaa" $ return ( ) 
        trace "aaa" $ return ( )
        print $ trace "aaa" $ "bbb"
        traceIO $ trace "aaa" $ "bbb"
        traceIO $ show $ isA 'A'
        putStrLn ""
        traceIO $ show $ isA 'A' || isA 'B'
        traceIO "---"
        traceIO $ show $ isA 'A' || isA 'A'
        traceIO "---"
        traceIO $ show $ isA 'B' || isA 'A'
        traceIO "---"
        traceIO $ show $ isA 'B' || isA 'C'
        putStrLn ""
        print $ Just 1 <|> Nothing
        print $ Just 1 <|> Just 2
        print $ Nothing <|> Just 2
        print ( Nothing <|> Nothing :: Maybe Int )
        putStrLn ""
        print $ [ 1 ] <|> [ ]
        print $ [ 1 ] <|> [ 2 ]
        print $ [ ] <|> [ 2 ]
        print ( [ ] <|> [ ] :: [ Int ] )
        putStrLn ""
        forM_ [ 1..5 ] $ \ x -> print $ check x
        putStrLn ""

    -- // ex_06
        putStrLn "// ex_06"
        print $ check3 "1"
        print $ check3 "2Ab"
        print $ check3 "Abc"
        print $ check3 "Ab1"
        print $ check3 "1AB"
        putStrLn ""

    -- // 
        putStrLn "// "
        putStrLn ""

-- // 

-- // ex_06
    check3 :: String -> Maybe String
    check3 str = do
        do
            guard $ isDigit $ str !! 0
            guard $ isUpper $ str !! 1
            <|> do
            guard $ isUpper $ str !! 0
            guard $ isLower $ str !! 1
        guard $ isLower $ str !! 2
        return str 

-- // Alternative
    isA :: Char -> Bool
    isA ch = trace ( show ch ) $ ch == 'A'

    check :: Int -> Maybe Int
    check x = do
        do
            guard $ x == 1
            <|> do
            guard $ x == 3
        return x

-- // ex_05
    numUpper :: Int -> Int -> String -> Maybe String
    numUpper x y s =  do
        guard $ x + y == length s
        guard $ x == length ( filter isDigit $ take x s )
        guard $ y == length ( filter isUpper $ drop y s )
        return s 

    numUpper' :: Int -> Int -> String -> Maybe String
    numUpper' x y s =
        let xs = take x s
            ys = drop x s
        in do
            guard $ x + y == length s
            guard $ foldr ( \ x bln -> isDigit x && bln ) True xs
            guard $ foldr ( \ x bln -> isUpper x && bln ) True ys
            return s 

-- // guard
    getch :: [ a ] -> Int -> Maybe a
    getch s n
        | n < length s = Just $ s !! n
        | otherwise = Nothing

    testStr' :: String -> Maybe String
    testStr' s = do
        ch0 <- getch s 0
        ch1 <- getch s 1
        ch2 <- getch s 2
        unless ( isUpper ch0 ) Nothing
        unless ( isLower ch1 ) Nothing
        unless ( isDigit ch2 ) Nothing
        return s

    testStr :: String -> Maybe String
    testStr s = do
        ch0 <- getch s 0
        ch1 <- getch s 1
        ch2 <- getch s 2
        guard ( isUpper ch0 )
        guard ( isLower ch1 )
        guard ( isDigit ch2 )
        return s

-- // ex_03
    mapMaybeL :: ( a -> Maybe b ) -> [ a ] -> [ b ]
    mapMaybeL f xs = [ x' | Just x' <- map f xs ]

    mapMaybeR :: ( Eq b ) => ( a -> Maybe b ) -> [ a ] -> [ b ]
    mapMaybeR f [ ] = [ ]
    mapMaybeR f ( x : xs ) = case f x of
        Just y -> y : mapMaybeR f xs
        Nothing -> mapMaybeR f xs
        
{-
        | f x == Nothing = mapMaybeR f xs 
        | otherwise =
            let  Just x' = f x
            in x' : ( mapMaybeR f xs )
-}
    
    mapMaybeF :: ( a -> Maybe b ) -> [ a ] -> [ b ]
    mapMaybeF f = ( `foldr` [ ] ) $
        \ x acc -> case f x of
            Just y -> y : acc
            Nothing -> acc


-- // mapMaybe
    facts :: Int -> Maybe ( Int, Int, Int )
    facts n = do
        a <- fact n
        b <- fact ( n - 1 )
        c <- fact ( n - 2 )
        return ( a, b, c )

    factsLs :: Int -> ( [ Maybe Int ], [ Int ] )
    factsLs n = ( map fact [ n, n - 1, n - 2 ]
                , mapMaybe fact [ n, n - 1, n - 2 ])

-- // ex_02
    bindMb :: Maybe a -> ( a -> Maybe b ) -> Maybe b
    bindMb ( Just x ) f = f x
    bindMb Nothing f = Nothing

-- // Nothing
    test :: Maybe Int -> Int
    test ( Just x ) = x
    test Nothing = 0

-- // ex_01
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n | n > 1 = fib ( n - 1 ) + fib ( n - 2 )

    fibMb :: Int -> Maybe Int
    fibMb 0 = Just 0
    fibMb 1 = Just 1
    fibMb n
        | n > 1 = ( + ) <$> fibMb ( n - 1 ) <*> fibMb ( n - 2 )
        | otherwise = Nothing

-- // fact
    fact :: Int -> Maybe Int
    fact 0 = Just 1
    fact n
        | n > 0 = ( n * ) <$> fact ( n - 1 )
        | otherwise = Nothing


