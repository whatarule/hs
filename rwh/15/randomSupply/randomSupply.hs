
    module RandomSupply where

    import Supply
    import System.Random hiding ( next )

--  randomsIO :: Random a => IO [ a ]
    randomsIO :: IO [ Int ]
    randomsIO =
        getStdRandom $ \ g ->
            let ( a, b ) = split g
            in ( randoms a, b )


    main = do
        
    --  r <- randomsIO :: IO [ Int ]
    --  print $ r
        r <- ( fst . runSupply next ) `fmap` randomsIO
        print $ r
        print $ ( )



