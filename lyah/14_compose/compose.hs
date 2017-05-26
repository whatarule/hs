
    import Control.Monad
    import Data.List

    main = do
    
    -- // <=<
        let f = ( + 1 ) . ( * 100 )
        print ( f 4 )
        let g = ( \ x -> return ( x + 1 ) )
                <=< ( \ x -> return ( x * 100 ) )
        print ( Just 4 >>= g )
        let f' = foldr (.) id [ ( + 8 ), ( * 100 ), ( + 1 ) ]
        print ( f' 1 )
        let g' = foldr (<=<) return [ ( \ x -> return ( x + 8 ) ), 
                                      ( \ x -> return ( x * 100 ) ), 
                                      ( \ x -> return ( x + 1 ) ) ]
        print ( Just 1 >>= g' )
        print (  )

    -- // KnightPos
        print ( inMany 1 ( 6, 2 ) )
        print ( canReachIn 1 ( 6, 2 ) ( 8, 1 ) )
        print ( canReachIn 3 ( 6, 2 ) ( 1, 1 ) )
        print ( canReachIn 100 ( 6, 2 ) ( 1, 1 ) )
        print (  )


    type KnightPos = ( Int, Int )

    moveKnight :: KnightPos -> [ KnightPos ]
    moveKnight ( c, r ) = do
        p <- [ 2, -2 ]
        q <- [ -1, 1 ]
        s <- [ [ ( p, q ) ] ]
        t <- [ [ ( q, p ) ] ]
        let u = s ++ t
        ( a, b ) <- u
        ( c', r' ) <- [ ( c + a, r + b ) ]
        guard ( c' `elem` [ 1..8 ] && r' `elem` [ 1..8 ] )
        return ( c', r' )

    inMany :: Int -> KnightPos -> [ KnightPos ]
    inMany x start = return start
                      >>= foldr (<=<) return
                          ( replicate x moveKnight )

    canReachIn :: Int -> KnightPos -> KnightPos -> Bool
    canReachIn x start end = end `elem` inMany x start


    in3 :: KnightPos -> [ KnightPos ]
{-
    in3 start = do
        first <- moveKnight start
        second <- moveKnight first
        moveKnight second
-}
    in3 start = return start
                >>= moveKnight
                >>= moveKnight
                >>= moveKnight
{-
    in3 start = moveKnight <=<
                moveKnight <=<
                moveKnight $ start
-}

    canReachIn3 :: KnightPos -> KnightPos -> Bool
    canReachIn3 start end = end `elem` in3 start


