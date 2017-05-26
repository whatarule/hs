
    import Control.Exception
    import Data.Char

    import Control.Monad.State
    import Control.Monad.Except

    main = do

        putStrLn ""

    -- // anyChar
        putStrLn "// anyChar"
        print $ anyChar "abc"
        print $ anyCharT "abc"
        let ( x1, xs1 ) = anyCharT "abc"
            ( x2, xs2 ) = anyCharT xs1
        print $ [ x1, x2 ]
        print $ test01 "abc" 
        putStrLn ""
        print $ anyCharT "abc" 
        print $ test01T "abc" 
        print $ test02T "abc" 
        putStrLn ""
        parseTest test02T "12" 
        parseTest test02T "123" 
        putStrLn ""
        parseTest ( satisfy ( == 'a' ) ) "abc"
        parseTest ( satisfy ( == 'a' ) ) "123"
        parseTest ( satisfy isDigit ) "abc"
        parseTest ( satisfy isDigit ) "123"
        putStrLn ""
        parseTest ( char 'a' ) "abc"
        parseTest ( char 'a' ) "123"
        parseTest digit "abc"
        parseTest digit "123"
        parseTest letter "abc"
        parseTest letter "123"
        putStrLn ""
        parseTest test03T "abc"
        parseTest test03T "123"
        parseTest test03T "a23"
        parseTest test03T "a234"
        putStrLn ""

    -- // State 
    -- // ex_01
        putStrLn "// State"
        putStrLn "// ex_01"
        print $ runState anyCharSt "abc"
        print $ runState test01St "abc"
        print $ runState test02St "abc"
        parseTestSt test02St "123"
        parseTestSt test02St "12"
        putStrLn ""
        parseTestSt ( satisfySt ( == 'a' ) ) "abc"
        parseTestSt ( satisfySt ( == 'a' ) ) "123"
        parseTestSt ( satisfySt isDigit ) "abc"
        parseTestSt ( satisfySt isDigit ) "123"
        putStrLn ""
        parseTestSt ( charSt 'a' ) "abc"
        parseTestSt ( charSt 'a' ) "123"
        parseTestSt digitSt "abc"
        parseTestSt digitSt "123"
        parseTestSt letterSt "abc"
        parseTestSt letterSt "123"
        putStrLn ""
        parseTestSt test03St "abc"
        parseTestSt test03St "123"
        parseTestSt test03St "a23"
        parseTestSt test03St "a234"
        putStrLn ""

    -- // sequence
        putStrLn "// sequence"
        print $ runState test01Sq "abc"
        print $ runState test02Sq "abc"
        parseTestSt test02Sq "123"
        parseTestSt test02Sq "12"
        putStrLn ""
        parseTestSt test03Sq "abc"
        parseTestSt test03Sq "123"
        parseTestSt test03Sq "a23"
        parseTestSt test03Sq "a234"
        putStrLn ""

    -- // Maybe
    -- // ex_02
        putStrLn "// Maybe"
        putStrLn "// ex_02"
        print $ anyCharMb "abc"
        print $ anyCharMb "a"
        print $ anyCharMb ""
        print $ test01Mb "abc" 
        print $ test01Mb "a" 
        print $ test02Mb "abc" 
        print $ test02Mb "ab" 
        putStrLn ""
        parseTestMb test02Mb "12" 
        parseTestMb test02Mb "123" 
        putStrLn ""
        parseTestMb ( satisfyMb ( == 'a' ) ) "abc"
        parseTestMb ( satisfyMb ( == 'a' ) ) "123"
        parseTestMb ( satisfyMb ( == 'a' ) ) ""
        parseTestMb ( satisfyMb isDigit ) "abc"
        parseTestMb ( satisfyMb isDigit ) "123"
        parseTestMb ( satisfyMb isDigit ) ""
        putStrLn ""
        parseTestMb ( charMb 'a' ) "abc"
        parseTestMb ( charMb 'a' ) "123"
        parseTestMb digitMb "abc"
        parseTestMb digitMb "123"
        parseTestMb letterMb "abc"
        parseTestMb letterMb "123"
        putStrLn ""
        parseTestMb test03Mb "abc"
        parseTestMb test03Mb "123"
        parseTestMb test03Mb "a23"
        parseTestMb test03Mb "a234"
        putStrLn ""

    -- // Either
        putStrLn "// Either"
        print $ anyCharEi "abc"
        print $ anyCharEi "a"
        print $ anyCharEi ""
        print $ test01Ei "abc" 
        print $ test01Ei "a" 
        print $ test02Ei "abc" 
        print $ test02Ei "ab" 
        putStrLn ""
        parseTestEi test02Ei "12" 
        parseTestEi test02Ei "123" 
        putStrLn ""
        parseTestEi ( satisfyEi ( == 'a' ) ) "abc"
        parseTestEi ( satisfyEi ( == 'a' ) ) "123"
        parseTestEi ( satisfyEi ( == 'a' ) ) ""
        parseTestEi ( satisfyEi isDigit ) "abc"
        parseTestEi ( satisfyEi isDigit ) "123"
        parseTestEi ( satisfyEi isDigit ) ""
        putStrLn ""
        parseTestEi ( charEi 'a' ) "abc"
        parseTestEi ( charEi 'a' ) "123"
        parseTestEi digitEi "abc"
        parseTestEi digitEi "123"
        parseTestEi letterEi "abc"
        parseTestEi letterEi "123"
        putStrLn ""
        parseTestEi test03Ei "abc"
        parseTestEi test03Ei "123"
        parseTestEi test03Ei "a23"
        parseTestEi test03Ei "a234"
        putStrLn ""

    -- // ExceptT
        putStrLn "// ExceptT"
        print $ runCharEiT anyCharEiT "abc"
        print $ runCharEiT anyCharEiT "a"
        print $ runCharEiT anyCharEiT ""
        print $ runCharEiT test01EiT "abc" 
        print $ runCharEiT test01EiT "a" 
        print $ runCharEiT test02EiT "abc" 
        print $ runCharEiT test02EiT "ab" 
        putStrLn ""

    -- // StateT
        putStrLn "// StateT"
        print $ runStateT anyCharStT "abc"
        print $ runStateT anyCharStT "a"
        print $ runStateT anyCharStT ""
        print $ runStateT test01StT "abc"
        print $ runStateT test01StT "a"
        print $ runStateT test02StT "abc"
        print $ runStateT test02StT "ab"
        putStrLn ""
        parseTestStT test02StT "123"
        parseTestStT test02StT "12"
        putStrLn ""
        parseTestStT ( satisfyStT ( == 'a' ) ) "abc"
        parseTestStT ( satisfyStT ( == 'a' ) ) "123"
        parseTestStT ( satisfyStT isDigit ) "abc"
        parseTestStT ( satisfyStT isDigit ) "123"
        putStrLn ""
        parseTestStT ( charStT 'a' ) "abc"
        parseTestStT ( charStT 'a' ) "123"
        parseTestStT digitStT "abc"
        parseTestStT digitStT "123"
        parseTestStT letterStT "abc"
        parseTestStT letterStT "123"
        putStrLn ""
        parseTestStT test03StT "abc"
        parseTestStT test03StT "123"
        parseTestStT test03StT "a23"
        parseTestStT test03StT "a234"
        putStrLn ""
        parseTestStT test04StT "a"
        parseTestStT test04StT "1"
        parseTestStT test04StT "!"
        putStrLn ""
        parseTestStT test05StT "a123"
        parseTestStT test05StT "ab123"
        parseTestStT test06StT "a123"
        parseTestStT test06StT "ab123"
        putStrLn ""

    -- // ex_03
        putStrLn "// ex_03"
    --  <$> :: ( Functor f ) => ( a -> b ) -> f a -> f b
    --  <*> :: ( Applicative f ) => f ( a -> b ) -> f a -> f b
        parseTestStT test07StT "abc123"
        parseTestStT test07StT "123abc"
        parseTestStT test08StT "abc123"
        parseTestStT test08StT "123abc"
        putStrLn ""

    -- // 
        putStrLn "// "
        putStrLn ""

-- // 

-- // ex_03
    altStT :: StateT String ( Either String ) String -> StateT String ( Either String ) String -> StateT String ( Either String ) String
    StateT f `altStT` StateT g = StateT $ \ s -> ( f s ) `altEi` ( g s )

    manyStT :: StateT String ( Either String ) Char -> StateT String ( Either String ) [ Char ]
    manyStT p = ( : ) <$> p <*> manyStT p `altStT` return [ ]

    test07StT :: StateT String ( Either String ) String
    test07StT = manyStT letterStT

    test08StT :: StateT String ( Either String ) String
    test08StT = manyStT test04StT

-- // StateT
    anyCharStT :: StateT [ a ] ( Either String ) a
    anyCharStT = StateT anyCharEi

    test01StT :: StateT [ a ] ( Either String ) [ a ]
    test01StT = sequence [ anyCharStT, anyCharStT ]

    test02StT :: StateT [ a ] ( Either String ) [ a ]
    test02StT = ( ++ ) <$> test01StT <*> sequence [ anyCharStT ]

    parseTestStT :: ( Show a, Show b ) => StateT [ a ] ( Either String ) b -> [ a ] -> IO ( )
    parseTestStT p s = case evalStateT p s of
        Right x -> print x
        Left e -> putStrLn $ "[" ++ show s ++ "] " ++ e

    satisfyStT :: ( Show a ) => ( a -> Bool ) -> StateT [ a ] ( Either String ) a
    satisfyStT f = StateT $ satisfyEi f

    charStT :: Char -> StateT String ( Either String ) Char
    charStT c = StateT $ charEi c

    digitStT :: StateT String ( Either String ) Char
    digitStT = StateT $ digitEi

    letterStT :: StateT String ( Either String ) Char
    letterStT = StateT $ letterEi

    test03StT :: StateT String ( Either String ) String
    test03StT = sequence [ letterStT, digitStT, digitStT ]

    test04StT :: StateT String ( Either String ) Char
    test04StT = StateT $ \ xs -> ( letterEi xs `altEi` digitEi xs )

    test05StT :: StateT String ( Either String ) String
    test05StT = sequence [ letterStT, digitStT, digitStT, digitStT ]

    test06StT :: StateT String ( Either String ) String
    test06StT = sequence $ letterStT : replicate 3 digitStT

-- // ExceptT
    anyCharEiT :: ExceptT String ( State [ a ] ) a
    anyCharEiT = do
        s <- get
        case s of
            ( x : xs ) -> do
                put xs
                return x
            _ ->  throwError "too short"

    runCharEiT :: ExceptT String ( State [ a ] ) b -> [ a ] -> ( Either String b, [ a ] )
    runCharEiT c s = ( `runState` s ) $ runExceptT c

    test01EiT :: ExceptT String ( State [ a ] ) [ a ]
    test01EiT = do
        x1 <- anyCharEiT
        x2 <- anyCharEiT
        return [ x1, x2 ]

    test02EiT :: ExceptT String ( State [ a ] ) [ a ]
    test02EiT = do
        x1 <- test01EiT
        x2 <- anyCharEiT
        return $ x1 ++ [ x2 ]

-- // Either
    anyCharEi :: [ a ] -> Either String ( a, [ a ] )
    anyCharEi ( x : xs ) = Right ( x, xs )
    anyCharEi _ = Left "too short"

    test01Ei :: [ a ] -> Either String ( [ a ], [ a ] )
    test01Ei xs0 = do
        ( x1, xs1 ) <- anyCharEi xs0
        ( x2, xs2 ) <- anyCharEi xs1
        return ( [ x1, x2 ], xs2 )

    test02Ei :: [ a ] -> Either String ( [ a ], [ a ] )
    test02Ei xs0 = do
        ( x1, xs1 ) <- test01Ei xs0
        ( x2, xs2 ) <- anyCharEi xs1
        return $ ( x1 ++ [ x2 ], xs2 )

    parseTestEi :: ( Show a, Show b ) => ( [ a ] -> Either String ( b, [ a ] ) ) -> [ a ] -> IO ( )
    parseTestEi p s = case p s of
        Right ( x, _ ) -> print x
        Left e -> putStrLn $ "[" ++ show s ++ "] " ++ e

    satisfyEi :: ( Show a ) => ( a -> Bool ) -> [ a ] -> Either String ( a, [ a ] )
    satisfyEi f ( x : xs ) | not $ f x  = Left $ show x
    satisfyEi f x  = anyCharEi x

    altEi :: Either String ( b, [ a ] ) -> Either String ( b, [ a ] ) -> Either String ( b, [ a ] )
    Left a `altEi` Left b = Left $ b ++ a
    Left a `altEi` b = b
    a `altEi` b = a 

    charEi :: Char -> String -> Either String ( Char, String )
    charEi c xs = satisfyEi ( == c ) xs `altEi` Left ( "not char " ++ show c ++ ": ")

    digitEi :: String -> Either String ( Char, String )
    digitEi xs = satisfyEi isDigit xs `altEi` Left "not digit: "

    letterEi :: String -> Either String ( Char, String )
    letterEi xs = satisfyEi isLetter xs `altEi` Left "not letter: "

    test03Ei :: String -> Either String ( String, String )
    test03Ei xs0 = do
        ( x1, xs1 ) <- letterEi xs0
        ( x2, xs2 ) <- digitEi xs1
        ( x3, xs3 ) <- digitEi xs2
        return ( [ x1, x2, x3 ], xs3 )
{-
    manyEi :: ( String -> Either String ( Char, String ) ) -> ( String -> Either String [ Char ] )
    manyEi p = ( : ) <$> p <*> ( manyEi p <|> ( \ _ -> return [ ] ) )
-}

-- // Maybe
-- // ex_02
    anyCharMb :: [ a ] -> Maybe ( a, [ a ] )
    anyCharMb ( x : xs ) = Just ( x, xs )
    anyCharMb _ = Nothing

    test01Mb :: [ a ] -> Maybe ( [ a ], [ a ] )
    test01Mb xs0 = do
        ( x1, xs1 ) <- anyCharMb xs0
        ( x2, xs2 ) <- anyCharMb xs1
        return ( [ x1, x2 ], xs2 )

    test02Mb :: [ a ] -> Maybe ( [ a ], [ a ] )
    test02Mb xs0 = do
        ( x1, xs1 ) <- test01Mb xs0
        ( x2, xs2 ) <- anyCharMb xs1
        return $ ( x1 ++ [ x2 ], xs2 )

    parseTestMb :: ( Show a, Show b ) => ( [ a ] -> Maybe ( b, [ a ] ) ) -> [ a ] -> IO ( )
    parseTestMb p s = case p s of
        Just ( x, _ ) -> print x
        n -> print n
{-
    parseTestMb p s = do
        print $ ( fmap fst ) $ p s
        `catch` \ ( SomeException e ) ->
            putStr $ show e
-}    

    satisfyMb :: ( a -> Bool ) -> [ a ] -> Maybe ( a, [ a ] )
    satisfyMb f ( x : xs ) | not $ f x = Nothing
    satisfyMb f xs = anyCharMb xs

    charMb :: Char -> String -> Maybe ( Char, String )
    charMb c = satisfyMb ( == c )

    digitMb :: String -> Maybe ( Char, String )
    digitMb = satisfyMb isDigit

    letterMb :: String -> Maybe ( Char, String )
    letterMb = satisfyMb isLetter

    test03Mb :: String -> Maybe ( String, String )
    test03Mb xs0 = do
        ( x1, xs1 ) <- letterMb xs0
        ( x2, xs2 ) <- digitMb xs1
        ( x3, xs3 ) <- digitMb xs2
        return ( [ x1, x2, x3 ], xs3 )

-- // sequence
    test01Sq :: State [ a ] [ a ]
    test01Sq = sequence [ anyCharSt, anyCharSt ]

    test02Sq :: State [ a ] [ a ]
    test02Sq = ( ++ ) <$> test01St <*> sequence [ anyCharSt ]

    test03Sq :: State String String
    test03Sq = sequence [ letterSt, digitSt, digitSt ]

-- // State 
-- // ex_01
    anyCharSt :: State [ a ] a
    anyCharSt = state $ anyChar
        where anyChar ( x : xs ) = ( x, xs )
--  anyCharSt = state $ \ ( x : xs ) -> ( x, xs ) 
{-
    anyCharSt = do
        ( x : xs ) <- get
        put xs
        return x
-}

    test01St :: State [ a ] [ a ]
    test01St = do
        x1 <- anyCharSt
        x2 <- anyCharSt
        return [ x1, x2 ]

    test02St :: State [ a ] [ a ]
    test02St = do
        x1 <- test01St
        x2 <- anyCharSt
        return $ x1 ++ [ x2 ]

    parseTestSt :: ( Show a, Show b ) => State [ a ] b -> [ a ] -> IO ( )
    parseTestSt p s = do
        print $ evalState p s
        `catch` \ ( SomeException e ) ->
            putStr $ show e

    satisfySt :: ( a -> Bool ) -> State [ a ] a
    satisfySt f = state $ satisfy f
        where satisfy f ( x : xs ) | f x = runState anyCharSt ( x : xs )
{-
    satisfySt f = do
        ( x : xs ) <- get
        case f x of
            True -> do
                put xs
                return x
-}

    charSt :: Char -> State String Char
    charSt c = satisfySt ( == c )

    digitSt :: State String Char
    digitSt = satisfySt isDigit

    letterSt :: State String Char
    letterSt = satisfySt isLetter

    test03St :: State String String
    test03St = do
        x1 <- letterSt
        x2 <- digitSt
        x3 <- digitSt
        return [ x1, x2, x3 ]

    manySt :: State String Char -> State String [ Char ]
    manySt p = ( : ) <$> p <*> return [ ]


-- // anyChar
    anyChar :: [ a ] -> a
    anyChar ( x : _ ) = x

    anyCharT :: [ a ] -> ( a, [ a ] )
    anyCharT ( x : xs ) = ( x, xs )

    test01 :: [ a ] -> [ a ]
    test01 xs0 =
        let ( x1, xs1 ) = anyCharT xs0
            ( x2, xs2 ) = anyCharT xs1
        in [ x1, x2 ]

    test01T :: [ a ] -> ( [ a ], [ a ] )
    test01T xs0 =
        let ( x1, xs1 ) = anyCharT xs0
            ( x2, xs2 ) = anyCharT xs1
        in ( [ x1, x2 ], xs2 )

    test02T :: [ a ] -> ( [ a ], [ a ] )
    test02T xs0 =
        let ( x1, xs1 ) = test01T xs0
            ( x2, xs2 ) = anyCharT xs1
        in ( x1 ++ [ x2 ], xs2 )

    parseTest :: ( Show a, Show b ) => ( [ a ] -> ( b, [ a ] ) ) -> [ a ] -> IO ( )
    parseTest p s = do
        print $ fst $ p s
        `catch` \ ( SomeException e ) ->
            putStr $ show e

    satisfy :: ( a -> Bool ) -> [ a ] -> ( a, [ a ] )
    satisfy f ( x : xs ) | f x = ( x, xs )

    char :: Char -> String -> ( Char, String )
    char c = satisfy ( == c )

    digit :: String -> ( Char, String )
    digit = satisfy isDigit

    letter :: String -> ( Char, String )
    letter = satisfy isLetter

    test03T :: String -> ( String, String )
    test03T xs0 =
        let ( x1, xs1 ) = letter xs0
            ( x2, xs2 ) = digit xs1
            ( x3, xs3 ) = digit xs2
        in ( [ x1, x2, x3 ], xs3 )


