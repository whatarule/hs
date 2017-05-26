
    import System.Random
    import Control.Monad.State

    get' :: State s s
    get' = state $ \ s -> ( s, s )

    type RandomState a = State StdGen a

    main = do

    -- // twoRandoms
        r <- getStdGen
        print r
        r <- getStdGen
        print r
        r <- twoBadRandoms `fmap` getStdGen
        print r
        r <- twoGoodRandoms `fmap` getStdGen
        print r
        print $ ( ) 

    -- // State
        print $ mkStdGen 1
        print $ ( random ( mkStdGen 1 ) :: ( Int, StdGen ) )
        print $ runState ( state ( \ s -> ( 1, s ) ) ) $ 2
    --  let s = state ( \ s -> ( 1, s ) ) 
        print $ runState get $ 1 
        print $ runState ( put 1 ) $ 2
        print $ ( )

    -- // getRandom
        print $ ( runState getRandom ( mkStdGen 1 ) :: ( Int, StdGen ) )
        r <- runState getRandom `fmap` getStdGen :: IO ( ( Int, StdGen ) )
        print r
        print $ ( , ) 1 2
        r <- runState getTwoRandoms `fmap` getStdGen 
        print r
        r <- runTwoRandoms
        print r
        print $ ( ) 

    -- // CountedRandom
        print $ CountedRandom ( mkStdGen 1 ) 0
        let c = CountedRandom ( mkStdGen 1 ) 0
        print c
        print $ runState ( state ( \ s -> ( 1, s ) ) ) $ c 
        let f = \ s -> ( 1, s )
        print $ runState ( state f ) $ c
        let cr = runState ( state f ) $ c
        print cr
        print $ runState getCountedRandom $ c
        print $ ( runState getCountedRandom ) ( snd $ runState getCountedRandom $ c )
        print $ runState getCount $ c
        print $ runState ( putCount 1 ) $ c
        print $ runState ( putCountModify 1 ) $ c
        print $ ( )


    data CountedRandom = CountedRandom {
        crGen :: StdGen ,
        crCount :: Int
      } deriving Show

    type CRState = State CountedRandom

    getCountedRandom :: CRState Int
--  getCountedRandom :: State CountedRandom Int
    getCountedRandom = do
        st <- get
        let ( val, gen1 ) = random ( crGen st )
        put CountedRandom { crGen = gen1, crCount = crCount st + 1 }
        return val

    getCount :: CRState Int
    getCount = crCount `liftM` get

    putCount :: Int -> CRState ( )
    putCount a = do
        st <- get
        put st { crCount = a }

    putCountModify :: Int -> CRState ( )
    putCountModify a = modify $ \ st -> st { crCount = a }


-- // getRandom

    getRandom :: RandomState Int
--  getRandom :: State StdGen Int
    getRandom =
        get >>= \ gen0 ->
            let ( val, gen1 ) = random gen0 
            in put gen1 >> return val

    getTwoRandoms :: RandomState ( Int, Int )
    getTwoRandoms = liftM2 ( , ) getRandom getRandom

    runTwoRandoms :: IO ( Int, Int )
    runTwoRandoms = do
        oldState <- getStdGen
        let ( result, newState ) = runState getTwoRandoms oldState
        setStdGen newState
        return result

{-
    rand :: IO Int
    rand = getStdRandom ( randomR ( o, maxBound ) )
-}

    twoBadRandoms :: RandomGen g => g -> ( Int, Int )
    twoBadRandoms gen = ( fst $ random gen, fst $ random gen )

    twoGoodRandoms :: RandomGen g => g -> ( ( Int, Int ), g )
    twoGoodRandoms gen0 =
        let ( a, gen1 ) = random gen0
            ( b, gen2 ) = random gen1
        in ( ( a, b ), gen2 )


