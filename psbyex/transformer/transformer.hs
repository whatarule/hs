
    import Control.Monad.State
    import Control.Monad.Writer
    import Control.Monad.Reader
    import Control.Monad.Except
    import Control.Monad.Identity

    import Control.Applicative
--  import Text.Parsec hiding ( Parser, runParser, string )

    import Data.Char
    import qualified Data.Map as M hiding ( split )
    import qualified Data.Set as S hiding ( split )

    import Control.Monad.RWS
    import Control.Eff

    import System.Environment

    main = do
        
        putStrLn ""
        
    -- // MonadTrans
        putStrLn "// MonadTrans"
    {-
        class MonadTrans t where
            lift :: ( Monad m ) => m a -> t m a
    -}
        print $ runStateT split ""
        print $ runStateT split "test"
        print $ runStateT ( ( ++ ) <$> split <*> split ) "test"
        putStrLn ""

    -- // ExceptT 
        putStrLn "// ExceptT"
    {-
        class Monad m => MonadError e m | m -> e where
            throwError :: ( MonadError e m ) => e -> m a
            catchError :: ( MonadError e m ) => m a -> ( e -> m a ) -> m a
    -}
    --  runExceptT :: ExceptT e m a -> m ( Either e a )
        print $ runWriter $ runExceptT writerAndExceptT
        putStrLn ""

    -- // Parser
        putStrLn "// Parser"
        print $ runParser' splitWr' "test"
        print $ runParser' ( ( ++ ) <$> splitWr' <*> splitWr' ) "test"
        print $ runParser' splitWr' ""
        print $ runParser' ( ( ++ ) <$> splitWr' <*> splitWr' ) "t"
        putStrLn ""
        print $ runParser splitWr "test"
        print $ runParser ( ( ++ ) <$> splitWr <*> splitWr ) "test"
        print $ runParser splitWr ""
        print $ runParser ( ( ++ ) <$> splitWr <*> splitWr ) "t"
        putStrLn ""

    -- // safeDivide
        putStrLn "// safeDivide"
        print $ safeDivide 4 2
        print $ safeDivide 4 0
        putStrLn ""

    -- // string
        putStrLn "// string"
        print $ runParser ( string "abc" ) "abcdef"
        print $ runParser ( string "ab" ) "abcdef"
        print $ runParser ( string "xxx" ) "abcdef"
        putStrLn ""

    -- // Doc
        putStrLn "// Doc"
        render $ line "aaa"
        render $ indent $ line "aaa"
        render $ do
            indent $ line "aaa"
            indent $ do
                line "bbb"
                indent $ line "ccc"
        render $ do
            line "Here is some indended text:"
            indent $ do
                line "I am indented"
                line "So am I"
                indent $ line "I am even more indented"
        putStrLn ""

    -- // Alternative
        putStrLn "// Alternative"
    {-
        class ( functor f ) => Alt f where
            ( <|> ) :: f a -> f a -> f a
        class ( Alt f ) => Plus f where
            empty :: f a
        class ( Applicative f, Plus f ) => Alternartive f where
            many :: ( Alternative f ) => f a -> f [ a ]
            some :: ( Alternative f ) => f a -> f [ a ]
    -}
        print $ runParser ( many splitWr ) "test"
        print $ runParser ( many splitCh ) "test"
        putStrLn ""
    --  class ( Monad m, Alternative m ) <= MonadPlus m
    --  guard :: ( MonadPlus m ) => Bool -> m ( )
        print $ runParser upper "abcDEF"
        print $ runParser ( many upper ) "abcDEF"
        print $ runParser ( some upper ) "abcDEF"
        putStrLn ""
        print $ runParser upperOrLower "abcDEF"
        print $ runParser components "abCDeFgh"
        putStrLn ""

    -- // abString
        putStrLn "// abString"
        print $ runParser abString "aaabbb"
        print $ runParser abString "abbaab"
        print $ runParser abStringS "aaabbb"
        print $ runParser abStringS "abbaab"
        print $ runParser abStringL "abbaab"
        putStrLn ""

    -- // ParserExT
        putStrLn "// ParserExT"
        print $ runParserExT ( many splitWrExT ) "test"
        print $ runParserExT ( many splitChExT ) "test"
        putStrLn ""
        print $ runParserExT upperExT "abcDEF"
        print $ runParserExT ( many upperExT ) "abcDEF"
        print $ runParserExT ( some upperExT ) "abcDEF"
        putStrLn ""
        print $ runParserExT upperOrLowerExT "abcDEF"
        print $ runParserExT componentsExT "abCDeFgh"
        putStrLn ""

    -- // RWS
        putStrLn "// RWS"
    --  type RWS r w s = RWST r w s Identity
    --  runRWS :: RWS r w s a -> r -> s -> ( a, s, w )

    -- // Game
        putStrLn "// Game"
    -- // has
        putStrLn "// has"
        print $ runRWS ( do
            has Candle
            has Matches
            ) env00 state00
        putStrLn ""
    -- // pickUp
        putStrLn "// pickUp"
        print $ runRWS ( do
            pickUp Candle
            has Candle
            ) env00 state00
        putStrLn ""
        runGame $ do
            pickUp Matches
        putStrLn ""
        runGame $ do
            pickUp Candle
            pickUp Candle
        putStrLn ""
    -- // take
        putStrLn "// take"
        runGame $ do
            game [ "take", "Candle" ]
            game [ "take", "Matches" ]
            game [ "take", "Candle" ]
            game [ "take", "Knife" ]
        putStrLn ""
    -- // inventory
        putStrLn "// inventory"
        runGame $ do
            game [ "inventory" ]
            game [ "take", "Candle" ]
            game [ "inventory" ]
        putStrLn ""
    -- // use
        putStrLn "// use"
        runGame $ do
            game [ "use", "Candle" ]
            game [ "use", "Matches" ]
            game [ "take", "Candle" ]
            game [ "use", "Matches" ]
        putStrLn ""
    -- // look
        putStrLn "// look"
        runGame $ do
            game [ "look" ]
        putStrLn ""
    -- // move
        putStrLn "// move"
        runGame $ do
            game [ "north" ]
            game [ "look" ]
        putStrLn ""
        runGame $ do
            game [ "south" ]
            game [ "look" ]
        putStrLn ""
        runGame $ do
            game [ "east" ]
            game [ "look" ]
        putStrLn ""
        runGame $ do
            game [ "west" ]
            game [ "look" ]
        putStrLn ""
    -- // describe
        putStrLn "// describe"
        runGame $ do
            describe
            game [ "north" ]
            describe
            game [ "north" ]
            describe
        putStrLn ""

    -- // 
        putStrLn "// "
        putStrLn ""

-- // 


-- // Game
    env00 :: GameEnvironment
    env00 = GameEnvironment "Wataru" False

    items00 :: M.Map Coords ( S.Set GameItem )
    items00 = M.singleton ( Coords 0 0 ) ( S.singleton Candle )

    player00 :: Coords
    player00 = Coords 0 0

    inv00 :: S.Set GameItem
    inv00 = S.singleton Matches 

    state00 :: GameState
    state00 = GameState items00 player00 inv00

-- // RWS
    type PlayerName = String

    data GameEnvironment = GameEnvironment
        { playerName :: PlayerName
        , debugMode :: Bool
        }
    data Coords = Coords { x :: Int, y :: Int }
        deriving ( Show, Eq, Ord )
    data GameItem = Candle | Matches
        deriving ( Show, Eq, Ord )

    data GameState = GameState
        { items :: M.Map Coords ( S.Set GameItem )
        , player :: Coords
        , inventory :: S.Set GameItem
        }
        deriving ( Show, Eq, Ord )

    type Log = [ String ]
    type Game = RWS GameEnvironment Log GameState

    has :: GameItem -> Game Bool
    has item = do
        state <- get
        return $ item `S.member` ( inventory state )

    pickUp :: GameItem -> Game ( )
    pickUp item = do
        s <- get
        case ( player s) `M.lookup` ( items s ) of
            Just itemsP | item `S.member` itemsP -> do
                let newItems = M.update ( Just . ( S.delete item ) ) ( player s ) ( items s )
                    newInventory = S.insert item ( inventory s )
                    newGameState = GameState newItems ( player s ) newInventory
                put $ newGameState
                tell [ "You now have the " ++ show item ]
            _ -> tell [ "I don't see that item here." ]

    use :: GameItem -> Game ( )
    use Candle = do
        hasMatches <- has Matches
        case hasMatches of
            _ | hasMatches -> tell [ "Use Matches to light up the Candle." ]
            _ -> tell [ "You don't have anything to light it up with" ]
    use Matches = do
        hasCandle <- has Candle
        case hasCandle of
            _ | hasCandle -> do
                env <- ask
                tell [ "You light up the Candle."
                      , "Congratulations, " ++ ( playerName env ) ++ "!"
                      , "You win!"
                      ]
            _ | otherwise -> tell [ "You don't have anything to light." ]

    move :: Int -> Int -> Game ( )
    move dx dy = modify $ \ s ->
        let x0 = x $ player s
            y0 = y $ player s
            newPlayer = Coords ( x0 + dx ) ( y0 + dy )
        in GameState ( items s ) newPlayer ( inventory s ) 

    describe :: Game ( )
    describe = do
        state <- get
        case player state of
            Coords 0 0 -> tell [ "You are in a dark forest. You see a path to the north." ]
            Coords 0 1 -> tell [ "You are in a clearing." ]
            _ -> tell [ "You are deep in the forest." ]

    debug :: Game ( )
    debug = do
        env <- ask
        case env of
            _ | debugMode env -> do
                state <- get
                tell [ show state ]
            _ -> tell [ "Not running in debug mode." ]
{-
    dispatch :: IO ( )
    dispatch = do
        args <- getArgs
        game args
-}

    readItem :: String -> Maybe GameItem
    readItem "Candle" = Just Candle
    readItem "Matches" = Just Matches
    readItem _ = Nothing 

    game :: [ String ] -> Game ( )
    game [ "look" ] = do
        state <- get
        tell [ "You are at " ++ show ( player state ) ]

    game [ "take", item ] = case readItem item of
        Just gameItem -> pickUp gameItem
        Nothing -> tell [ "I don't know what item you are referring to." ] 

    game [ "inventory" ] = do
        s <- get
        tell $ ( \ item -> "You have the " ++ show item ++ "." ) <$> S.toList ( inventory s )

    game [ "use", item ] = case readItem item of
        Just gameItem -> do
            hasItem <- has gameItem
            case hasItem of
                _ | hasItem -> do
                    use gameItem
                _ -> tell [ "You don't have that item." ]

    game [ "north" ] = move 0 1
    game [ "south" ] = move 0 (-1)
    game [ "east" ] = move 1 0
    game [ "west" ] = move (-1) 0

    runGame :: Game a -> IO ( )
    runGame = \ g -> mapM_ putStrLn $ snd $ evalRWS g env00 state00


-- // ParserExT
    type ParserExT = ExceptT String ( StateT String ( Writer [ String ] ) )

    runParserExT :: ParserExT a -> String -> ( ( Either String a, String ), [ String ] )
    runParserExT p s = runWriter $ ( `runStateT` s ) $ runExceptT p

    splitWrExT'' :: ParserExT String 
    splitWrExT'' = do
        s <- lift $ get
        lift $ lift $ tell [ "The state is " ++ show s ]
        case s of
            "" -> throwError "Empty String"
            _ -> do
                lift $ put $ drop 1 s
                return $ take 1 s

    splitWrExT :: ParserExT String
    splitWrExT = do
        s <- get
        tell [ "The state is " ++ show s ]
        case s of
            "" -> throwError "Empty String"
            _ -> do
                put $ drop 1 s
                return $ take 1 s

    stringExT :: String -> ParserExT String
    stringExT pref = 
        let l = length pref
        in do
            s <- lift $ get
            lift $ lift $ tell [ "The state is " ++ show s ]
            if pref == take l s
                then do
                    lift $ put $ drop l s
                    return $ take l s
                else throwError "Didn't match the prefix"

    splitChExT :: ParserExT Char
    splitChExT = do
        s <- get
        tell [ "The state is " ++ show s ]
        case s of
            "" -> throwError "Empty String"
            _ -> do
                put $ tail s
                return $ head s

    upperExT :: ParserExT String 
    upperExT = do
        s <- splitChExT
        guard $ toUpper s == s
        return [ s ]

    lowerExT :: ParserExT String
    lowerExT = do
        s <- splitChExT
        guard $ toLower s == s
        return [ s ]

    upperOrLowerExT :: ParserExT [ String ]
    upperOrLowerExT = some upperExT <|> some lowerExT

    componentsExT :: ParserExT [ [ String ] ]
    componentsExT = many upperOrLowerExT

-- // abString
    abString :: Parser [ String ]
    abString = many ( string "a" <|> string "b" ) 

    abStringS :: Parser [ String ]
    abStringS = ( some $ string "a" ) <|> ( some $ string "b" )

    abStringL :: Parser [ [ String ] ]
    abStringL = many abStringS

-- // Alternative
    splitCh :: Parser Char
    splitCh = do
        s <- get
        tell [ "The state is " ++ show s ]
        case s of
            "" -> throwError "Empty String"
            _ -> do
                put $ tail s
                return $ head s

    upper :: Parser String 
    upper = do
        s <- splitCh
        guard $ toUpper s == s
        return [ s ]

    lower :: Parser String
    lower = do
        s <- splitCh
        guard $ toLower s == s
        return [ s ]

    upperOrLower :: Parser [ String ]
    upperOrLower = some upper <|> some lower

    components :: Parser [ [ String ] ]
    components = many upperOrLower

-- // Doc
    type Level = Int
    type Doc = ReaderT Level ( Writer [ String ] ) ( )

    line :: String -> Doc
    line str = do
        lv <- ask
        lift $ tell [ take lv ( repeat ' ' ) ++ str ]

    indent :: Doc -> Doc
    indent doc = do
        local ( + 2 ) doc

{-
    cat :: [ Doc ] -> Doc
    cat xs = unlines <$> sequence xs
-}

    render :: Doc -> IO ( )
    render = putStr . unlines . execWriter . ( `runReaderT` 0 )

-- // string
    string' :: String -> Parser String
    string' pref = 
        let l = length pref
        in do
            s <- get
            lift $ lift $ tell [ "The state is " ++ show s ]
            if pref == take l s
                then do
                    put $ drop l s
                    return $ take l s
                else lift $ throwError "Didn't match the prefix"
            
    string :: String -> Parser String
    string pref = 
        let l = length pref
        in do
            s <- get
            tell [ "The state is " ++ show s ]
            if pref == take l s
                then do
                    put $ drop l s
                    return $ take l s
                else throwError "Didn't match the prefix"
            

-- // safeDivide
    safeDivide :: Int -> Int -> Either String Int
    safeDivide n m = do
        case m of
            0 -> throwError "Divide by zero"
            _ -> return $ div n m 

-- // Parser
    type Parser' = StateT String ( WriterT [ String ] ( ExceptT String Identity ) )
    
    runParser' :: Parser' String -> String -> Either String ( ( String, String ), [ String ] ) 
    runParser' p s = runIdentity $ runExceptT $ runWriterT $ ( `runStateT` s ) p

    splitWr' :: Parser' String
    splitWr' = do
        s <- get
        lift $ tell [ "The state is " ++ show s ]
        case s of
            "" -> lift $ lift $ throwError "Empty string"
            _ -> do
                put $ drop 1 s
                return $ take 1 s

    type Parser = StateT String ( ExceptT String ( Writer [ String ] ) )

    runParser :: Parser a -> String -> ( Either String ( a, String ), [ String ] )
    runParser p s = runWriter $ runExceptT $ ( `runStateT` s ) p

    splitWr'' :: Parser String 
    splitWr'' = do
        s <- get
        lift $ lift $ tell [ "The state is " ++ show s ]
        case s of
            "" -> lift $ throwError "Empty String"
            _ -> do
                put $ drop 1 s
                return $ take 1 s

    splitWr :: Parser String
    splitWr = do
        s <- get
        tell [ "The state is " ++ show s ]
        case s of
            "" -> throwError "Empty String"
            _ -> do
                put $ drop 1 s
                return $ take 1 s


-- // ExceptT
    writerAndExceptT :: ExceptT String ( Writer [ String ] ) String
    writerAndExceptT = do
        tell [ "Before the error" ]
        throwError "Error!"
        tell [ "After the error" ]
        return "Return value"


-- // MonadTrans
    split :: StateT String ( Either String ) String
    split = do
        s <- get
        case s of
            "" -> lift $ Left "Empty String"
            _ -> do
                put $ drop 1 s
                return $ take 1 s



