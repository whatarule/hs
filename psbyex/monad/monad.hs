
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Writer

    main = do

        putStrLn ""
        
    -- // State
        putStrLn "// State"
    --  get :: State s s
    --  put :: s -> State s ( )
    --  modify :: ( s -> s ) -> State s ( )
    --  evalState :: State s a -> s -> a
    --  execState :: State s a -> s -> s
    --  runState :: State s a -> s -> ( a, s )
    --  sumArraySt [ 1..5 ] 
        print $ execState ( do
            sumArraySt [ 1, 2, 3 ] 
            sumArraySt [ 4, 5 ] 
            sumArraySt [ 6 ] 
            ) 0
        print $ evalState ( do
            sumArraySt [ 1, 2, 3 ] 
            sumArraySt [ 4, 5 ] 
            sumArraySt [ 6 ] 
            ) 0
        print $ runState ( do
            sumArraySt [ 1, 2, 3 ] 
            sumArraySt [ 4, 5 ] 
            sumArraySt [ 6 ] 
            ) 0
        putStrLn ""

    -- // testParens
        putStrLn "// testParens"
        print $ testParens ""
        print $ testParens "("
        print $ testParens ")"
        print $ testParens "()"
        print $ testParens "())"
        print $ testParens ")())"
        print $ testParens "(()("
        print $ testParens "((())())"
        putStrLn ""

    -- // Reader
        putStrLn "// Reader"
    --  ask :: Reader r r
    --  local :: ( r -> r ) -> Reader r a -> Reader r a
    --  runReader :: Reader r a -> r -> a
        putStrLn ""

    -- // Doc
        putStrLn "// Doc"
        let d = line "aaa"
        print $ runReader d 2
        print $ runReader ( indent d ) 0
        print $ ( `runReader` 0 ) $ d
        print $ ( `runReader` 0 ) $ sequence [ d, d ]
        render $ unlines <$> sequence [ d, d ]
        render $ cat [ line "a", indent $ line "b" ]
        render $ cat
            [ line "Here is some indended text:"
            , indent $ cat
                [ line "I am indented"
                , line "So am I"
                , indent $ line "I am even more indented"
                ]
            ]
        putStrLn ""

    -- // Writer
        putStrLn "// Writer"
    --  tell :: ( Monoid w ) => w -> Writer w ( )
    --  execWriter :: Writer w a -> w
    --  runWiter :: Writer w a -> ( a, w )
        print $ gcd' 18 45
        print $ gcd' 18 47
        putStrLn ""
        print $ execWriter $ gcdLog 18 45
        print $ runWriter $ gcdLog 18 47
        putStrLn ""

    -- // sumArray
        putStrLn "// sumArray"
        putStrLn ""

    -- // collatz 
        putStrLn "// collatz"
        print $ collatz 4
        print $ collatz 5
        putStrLn ""
        print $ ( `execState` 0 ) $ collatzSt 4
        print $ ( `execState` 0 ) $ collatzSt 5
        putStrLn ""
        print $ execWriter $ collatzWt 4
        print $ execWriter $ collatzWt 5
        putStrLn ""

    -- //  
        putStrLn "// "
        putStrLn ""

-- // 

-- // collatz
    collatz :: Int -> Int
    collatz n
        | n `mod` 2 == 0 = n `div` 2
        | n `mod` 2 == 1 = 3 * n + 1 

    collatzSt :: Int -> State Int Int
    collatzSt 1 = return 1
    collatzSt n = do
        modify ( + 1 )
        collatzSt $ collatz n
    
    collatzWt :: Int -> Writer [ Int ] ( ) 
    collatzWt 1 = return ( ) 
    collatzWt n =
        let m = collatz n
        in do
            tell [ m ]
            collatzWt m

-- // sumArray
{-
    sumArrayWt :: [ Int ] -> Writer ( Additive String ) ( )
    sumArrayWt = traverse $ ( \ n -> tell ( Additive n ) )
-}

    

-- // Writer
    gcd' :: Int -> Int -> Int
    gcd' n 0 = n
    gcd' 0 m = m
    gcd' n m = if n > m
        then gcd ( n - m ) m
        else gcd n ( m - n )

    gcdLog :: Int -> Int -> Writer [ String ] Int
    gcdLog n 0 = return n
    gcdLog 0 m = return m
    gcdLog n m = do
        tell [ "gcd " ++ show n ++ " " ++ show m ]
        if n > m
            then gcdLog ( n - m ) m
            else gcdLog n ( m - n )


-- // Doc
    type Level = Int
    type Doc = Reader Level String

    line :: String -> Doc
    line str = do
        lv <- ask
        return $ take lv ( repeat ' ' ) ++ str

    indent :: Doc -> Doc
    indent doc = do
        local ( + 2 ) doc

    cat :: [ Doc ] -> Doc
    cat xs = unlines <$> sequence xs

    render :: Doc -> IO ( )
    render = putStr . ( `runReader` 0 )

-- // Reader
{-
    hasPermission :: String -> Permissions -> Boolean
    addPermission :: String -> Permissions -> Permissions

    createUser :: Reader Permissions ( Maybe User )
    createUser = do
        permissions <- ask
        if hasPermission "admin" permissions
            then Just <$> newUser
            else return Nothing

    runAsAdmin :: Reader Permissions a -> Reader Permissions a
    runAsAdmin = local $ addPermission "admin"

    createUserAsAdmin :: Reader Permissions ( Maybe User )
    createUserAsAdmin = runAsAdmin createUser
-}

-- // testParens
    testParens :: String -> Bool
    testParens str
        | str == "" = True
        | ( head str ) == ')' = False
        | ( last str ) == '(' = False
        | str == "()" = True
        | ( length str ) `mod` 2 == 1 = False
        | otherwise =
            let
                h = head str 
                xs = init $ tail str
                t = last str
                xs' = [ if x == '(' then 1 else -1 | x <- xs ] 
                r = ( `execState` 0 ) $ traverse ( \ x -> modify ( + x ) ) xs'
            in if r == 0 then True else False

-- // State
    sumArray :: ( Num a ) => [ a ] -> a
    sumArray = foldl ( \ acc x -> acc + x ) 0

    sumArraySt :: [ Int ] -> State Int [ ( ) ]
    sumArraySt =
    --  let a = foldl ( \ acc x -> acc + x ) 0 xs
    --  in modify ( + a )
        traverse $ ( \ x -> modify ( + x ) )


