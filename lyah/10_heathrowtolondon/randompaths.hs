
    import System.Random
    import System.IO
    import Data.Char
    import System.Environment

    main = do

        argList <- getArgs
        if length argList == 3
        then do 
            let writtenStr = unlines $ map show $ randomPaths $ map read argList
            writeFile "10_heathrowtolondon/randompaths.txt"
                $ writtenStr
            putStrLn "written string:"
            putStr writtenStr
        else do
            putStrLn "The program takes exactly three arguments;"
            mapM_ putStrLn [ "a number of paths sets", "maximum length of a path", "an integer for random generator" ]


{-
--  randomR :: ( RandomGen g, Random a ) => ( a, a ) -> g -> ( a, g )
--  print ( randomR ( 1, 6 ) $ mkStdGen 359353 :: ( Int, StdGen ) )

    finiteRandoms :: ( Num n, Eq n, RandomGen g, Random a ) => n -> g -> ( [ a ], g )
    finiteRandoms 0 gen = ( [ ], gen )
    finiteRandoms n gen =
        let ( value, newGen ) = random gen
            ( restOfList, finalGen ) = finiteRandoms ( n - 1 ) newGen
        in ( value : restOfList, finalGen )
    
    main = do
        contents <- readFile "09_file/baabaa.txt"
        writeFile "09_file/baabaacaps.txt"
            $ map toUpper contents
-}

    randomPaths :: [ Int ] -> [ Int ] 
    randomPaths [ sets, long, n ] = 
    --  fst $ finiteRandomRs ( sets * 3 ) ( 0, long ) ( mkStdGen n )
        let ( a, newGenA ) = finiteRandomRs sets ( 0, long ) ( mkStdGen n )
            ( b, newGenB ) = finiteRandomRs sets ( 0, long ) newGenA
            ( c, newGenC ) = finiteRandomRs sets ( 0, long `div` 4 ) newGenB
        in conThree a b c

    conThree :: [ Int ] -> [ Int ] -> [ Int ] -> [ Int ]
    conThree [ ] [ ] [ ] = [ ]
    conThree xs ys zs =
        let listThree = ( head xs ) : ( head ys ) : ( head zs ) : [ ]
            xs' = tail xs
            ys' = tail ys
            zs' = tail zs
        in listThree ++ ( conThree xs' ys' zs' )

    finiteRandomRs :: ( Num n, Eq n, Random a, RandomGen g ) => n -> ( a, a ) -> g -> ( [ a ], g )
    finiteRandomRs 0 ( start, end ) gen = ( [ ], gen )
    finiteRandomRs n ( start, end ) gen =
        let ( value, newGen ) = randomR ( start, end ) gen
            ( restOfList, finalGen ) = finiteRandomRs ( n - 1 ) ( start, end ) newGen
        in ( value : restOfList, finalGen )


