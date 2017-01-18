
    import qualified Data.Map as Map

    main = do
        print ( Map.fromList [ ( 3, "shoes" ), ( 4, "trees" ), ( 9, "bees" ) ] )
        print ( Map.fromList [ ( "kima", "greggs" ), ( "jimmy", "mcnulty" ), ( "jay", "landsman" ) ] )
        print ( Map.fromList [ ( "MS", 1 ), ( "MS", 2 ), ( "MS", 3 ) ] )
        print (  )


    Map.fromList :: ( Ord k ) => [ ( k, v ) ] -> Map.Map k v




