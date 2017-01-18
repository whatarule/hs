
    main = do
        let lostNumbers  = [ 4, 8, 15, 16, 23, 42 ]
    --  let lostNumbers' = [ 1, 2, 3, 4, 'a', 'b', 'c' ]
        print ( lostNumbers )
        print ( [ 1, 2, 3, 4 ] ++ [ 9, 10, 11, 12 ] )
        print ( "hello" ++ " " ++ "world" )
        print ( [ 'w', 'o' ] ++ [ 'o', 't' ] )
        print ( [ "w", "o" ] ++ [ "o", "t" ] )
        print ( 'A' : " SMALL CAT" )
    --  print ( "A" : " SMALL CAT" )
    --  print ( [ 'A' ] : " SMALL CAT" )
        print ( 5 : [ 1, 2, 3, 4 ] )
        print ( [ 1, 2, 3, 4 ] ++ [ 5 ] )
    --  print ( [ 1, 2, 3, 4 ] ++ 5 )
        print ( 1 : 2 : 3 : [ ] )
        print ( "Steve Buscemi" !! 6 )
        print ( "Steve Buscemi" !! 5 )
        print ( [ 9.4, 33.2, 96.2, 11.2, 23.25 ] !! 1 )
        
        let b = [ [ 1, 2, 3, 4 ], [ 5, 3, 3, 3 ], [ 1, 2, 2, 3, 4 ], [1, 2, 3] ]
        print ( b )
        print ( b ++ [ [ 1, 1, 1, 1 ] ] )
        print ( [ 6, 6, 6 ] : b )
        print ( b !! 2 )
        
        print ( [ 3, 4, 2 ] < [ 3, 4, 3 ] )
        print ( [ 3, 2, 1 ] > [ 2, 1, 0 ] )
        print ( [ 3, 2, 1 ] > [ 2, 10, 100 ] )
        print ( [ 3, 4, 2 ] < [ 3, 4, 3 ] )
        print ( [ 3, 4, 2 ] > [ 2, 4 ] )
        print ( [ 3, 4, 2 ] == [ 3, 4, 2 ] )
        
        let c = [ 5, 4, 3, 2, 1 ]
        print ( head c )
        print ( tail c )
        print ( last c )
        print ( init c )
    --  print ( head [ ] )
        
        print ( length c )
        print ( null c )
        print ( reverse c )
        print ( take 3 c )
        print ( take 1 [ 3, 9, 3 ] )
        print ( take 5 [ 1, 2 ] )
        print ( take 0 [ 6, 6, 6 ] )
        print (  )



