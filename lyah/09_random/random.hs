
    import System.Random
    import Control.Monad( when )

--  random :: ( RandomGen g, Random a ) => g -> ( a, g )
--  mkStdGen :: Int -> StdGen

    randoms' :: ( RandomGen g, Random a ) => g -> [ a ]
    randoms' gen =
        let ( value, newGen ) = random gen
        in value : randoms' newGen

    finiteRandoms :: ( Num n, Eq n, RandomGen g, Random a ) => n -> g -> ( [ a ], g )
    finiteRandoms 0 gen = ( [ ], gen )
    finiteRandoms n gen =
        let ( value, newGen ) = random gen
            ( restOfList, finalGen ) = finiteRandoms ( n - 1 ) newGen
        in ( value : restOfList, finalGen )

--  randomR :: ( RandomGen g, Random a ) => ( a, a ) -> g -> ( a, g )
    

    main = do

    -- // random
        print ( randomNumber )
        print ( random $ mkStdGen 100 :: ( Int, StdGen ) )
        print ( random $ mkStdGen 949494 :: ( Int, StdGen ) )
        print ( random $ mkStdGen 949488 :: ( Float, StdGen ) )
        print ( random $ mkStdGen 949488 :: ( Bool, StdGen ) )
        print ( random $ mkStdGen 949488 :: ( Integer, StdGen ) )
        print (  )

    -- // threeCoins
        print ( threeCoins $ mkStdGen 21 )
        print ( threeCoins $ mkStdGen 22 )
        print ( threeCoins $ mkStdGen 943 )
        print ( threeCoins $ mkStdGen 944 )
        print (  )
        
    -- // randoms
        print ( take 5 $ randoms' $ mkStdGen 11 :: [ Int ] )
        print ( take 5 $ randoms' $ mkStdGen 11 :: [ Bool ] )
        print ( take 5 $ randoms' $ mkStdGen 11 :: [ Float ] )
        print ( finiteRandoms 5 $ mkStdGen 11 :: ( [ Int ], StdGen ))
        print (  )

    -- // randomR
        print ( randomR ( 1, 6 ) $ mkStdGen 359353 :: ( Int, StdGen ) )
        print ( randomR ( 1, 6 ) $ mkStdGen 3593533 :: ( Int, StdGen ) )
        print ( take 10 $ randomRs ( 'a', 'z' ) $ mkStdGen 3 :: [ Char ] )
        print (  )

    -- // getStdGen
        gen <- getStdGen
        putStrLn $ take 20 $ randomRs ( 'a', 'z' ) gen
    --  gen2 <- getStdGen
    --  putStrLn $ take 20 $ randomRs ( 'a', 'z' ) gen2
        gen2 <- newStdGen
        putStrLn $ take 20 $ randomRs ( 'a', 'z' ) gen2
        gen3 <- getStdGen
        putStrLn $ take 20 $ randomRs ( 'a', 'z' ) gen3
        print (  )

    -- // askForNumber
        gen <- getStdGen
        askForNumber gen
        print ( )


    askForNumber :: StdGen -> IO ( )
    askForNumber gen = do
        let ( randNumber, newGen ) = randomR ( 1, 10 ) gen :: ( Int, StdGen )
        putStrLn "Which number in the range from 1 to 10 am I thinking of?"
        numberString <- getLine
        when ( not $ null numberString ) $ do
        --  let number = read numberString
        --  if randNumber == number
            let number = reads numberString :: [ ( Int, String ) ]
            if randNumber == ( fst $ head number )
                then putStrLn "You are correct!"
                else do
                    putStrLn $ "Sorry, it was " ++ show randNumber
                    askForNumber newGen
{-
    askForNumber gen = do
        let ( randNumber, _ ) = randomR ( 1, 10 ) gen :: ( Int, StdGen )
        putStrLn "Which number in the range from 1 to 10 am I thinking of?"
        numberString <- getLine
        when ( not $ null numberString ) $ do
        --  let number = read numberString
        --  if randNumber == number
            let number = reads numberString :: [ ( Int, String ) ]
            if randNumber == ( fst $ head number )
                then putStrLn "You are correct!"
                else do
                    putStrLn $ "Sorry, it was " ++ show randNumber
                    newStdGen
                    main
-}


    randomNumber :: Int
    randomNumber = 4

    threeCoins :: StdGen -> ( Bool, Bool, Bool )
    threeCoins gen =
        let ( firstCoin, newGen ) = random gen
            ( secondCoin, newGen' ) = random newGen
            ( thirdCoin, newGen'' ) = random newGen'
        in ( firstCoin, secondCoin, thirdCoin )

