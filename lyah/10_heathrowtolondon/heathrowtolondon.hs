
    import Data.List

    data Section = Section { getA :: Int, getB :: Int, getC :: Int }
                    deriving ( Show )
    type RoadSystem = [ Section ]
    
    data Label = A | B | C deriving ( Show )
    type Path = [ ( Label, Int ) ]
    type Path' = ( Int, [ ( Label, Int ) ] )


    groupsOf :: Int -> [ a ] -> [ [ a ] ]
    groupsOf 0 _ = undefined
    groupsOf _ [ ] = [ ]
    groupsOf n xs = take n xs : groupsOf n ( drop n xs )

    main = do
    {-
        print ( roadStep ( [ ], [ ] ) ( head heathrowToLondon ) )
        print ( roadStep' ( ( 0, [ ] ), ( 0, [ ] ) ) ( head heathrowToLondon ) )
        print ( optimalPath heathrowToLondon )
        print (  )

        print ( groupsOf 3 [ 1..10 ] )
        print (  )
    -}

        contents <- getContents
        let threes = groupsOf 3 ( map read $ lines contents )
            roadSystem = map ( \ [ a, b, c ] -> Section a b c ) threes
            path = optimalPath roadSystem
            pathString = concat $ map ( show . fst ) path
            pathTime = sum $ map snd path
        putStrLn $ "The best path to take is: " ++ pathString
        putStrLn $ "Time taken: " ++ show pathTime
        
        putStrLn $ "Road system:"
        mapM_ putStrLn $ map show threes 


    heathrowToLondon :: RoadSystem
    heathrowToLondon =
      [ Section 50 10 30 ,
        Section 5 90 20,
        Section 40 2 25,
        Section 10 8 0 ]

    optimalPath :: RoadSystem -> Path
    optimalPath roadSystem =
        let ( bestAPath, bestBPath ) = foldl roadStep ( [ ], [ ] ) roadSystem
        in if sum ( map snd bestAPath ) <= sum ( map snd bestBPath )
            then reverse bestAPath
            else reverse bestBPath

    roadStep :: ( Path, Path ) -> Section -> ( Path, Path )
    roadStep ( pathA, pathB ) ( Section a b c ) =
        let timeA = sum ( map snd pathA )
            timeB = sum ( map snd pathB )
            forwardTimeToA = timeA + a
            crossTimeToA = timeB + b + c
            forwardTimeToB = timeB + b
            crossTimeToB = timeA + a + c
            newPathToA =
                if forwardTimeToA <= crossTimeToA
                    then ( A, a ) : pathA
                    else ( C, c ) : ( B, b ) : pathB
            newPathToB =
                if forwardTimeToB <= crossTimeToB
                    then ( B, b ) : pathB
                    else ( C, c ) : ( A, a ) : pathA
        in ( newPathToA, newPathToB )

    roadStep' :: ( Path', Path' ) -> Section -> ( Path', Path' )
    roadStep' ( ( timeA, pathA ), ( timeB, pathB ) ) ( Section a b c ) =
        let forwardTimeToA = timeA + a
            crossTimeToA = timeB + b + c
            forwardTimeToB = timeB + b
            crossTimeToB = timeA + a + c
            ( newPathToA, newTimeToA ) =
                if forwardTimeToA <= crossTimeToA
                    then  ( ( A, a ) : pathA ,
                            a + timeA )    
                    else  ( ( C, c ) : ( B, b ) : pathB ,
                            c + b + timeB )
            ( newPathToB, newTimeToB ) =
                if forwardTimeToB <= crossTimeToB
                    then  ( ( B, b ) : pathB ,
                            b + timeB )    
                    else  ( ( C, c ) : ( A, a ) : pathA ,
                            c + a + timeA )
        in ( ( newTimeToA, newPathToA ), ( newTimeToB, newPathToB ) )


