
    import Control.Monad

{-

    instance Monad [ ] where
        return x = [ x ]
        xs >>= f = concat ( map f xs )
        fail _ = [ ]

    class Monad m => MonadPlus m where
        mzero :: m a
        mplus :: m a -> m a -> m a

    instance MonadPlus [ ] where
        mzero = [ ]
        mplus = ( ++ )

    guard :: ( MonadPlus m ) => Bool -> m ( )
    guard True = return ( )
    guard False = mzero

-}

    main = do
       
    -- // >>=
        print ( ( * ) <$> [ 1, 2, 3 ] <*> [ 10, 100, 1000 ] )
        print ( [ 3, 4, 5 ] >>= \ x -> [ x, -x ] )
        print ( [ ] >>= \ x -> [ "bad", "mad", "rad" ] :: [ String ])
        print ( [ 1, 2, 3 ] >>= \ x -> [ ] :: [ Int ] )
        print ( [ 1, 2 ] >>= \ n -> [ 'a', 'b' ] >>= \ ch -> return ( n, ch ) )
        print ( listOfTaples )
        print ( listOfTaples' [ 1, 2 ] [ 'a', 'b' ] )
        print ( [ (n, ch) | n <- [ 1, 2 ], ch <- [ 'a', 'b' ] ] )
        print (  )

    -- // guard
        print ( [ x | x <- [ 1..50 ], '7' `elem` show x ] )
        print ( guard ( 5 > 2 ) :: Maybe ( ) )
        print ( guard ( 1 > 2 ) :: Maybe ( ) )
        print ( guard ( 5 > 2 ) :: [ ] ( ) )
        print ( guard ( 1 > 2 ) :: [ ] ( ) )
        print ( [ 1..50 ] >>= ( \ x -> guard ( '7' `elem` show x ) >> return x ) )
        print ( guard ( 5 > 2 ) >> return "cool" :: [ String ] )
        print ( guard ( 1 > 2 ) >> return "cool" :: [ String ] )
        print ( guard ( 1 > 2 ) >> return "cool" :: [ String ] )
        print ( sevensOnly )
        print ( sevensOnly' [ 1..50 ] )
        print (  )

    -- // knightPos
        print ( moveKnight ( 6, 2 ) )
        print ( moveKnight ( 8, 1 ) )
        print ( ( 6, 2 ) `canReachIn3` ( 6, 1 ) )
        print ( ( 6, 2 ) `canReachIn3` ( 7, 3 ) )
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


    listOfTaples :: [ ( Int, Char ) ]
    listOfTaples = do
        n <- [ 1, 2 ]
        ch <- [ 'a', 'b' ]
        return ( n, ch )

    listOfTaples' :: ( Monad m ) => m a -> m b -> m ( a, b ) 
    listOfTaples' xs ys = do
        n <- xs
        ch <- ys
        return ( n, ch )

    sevensOnly :: [ Int ]
    sevensOnly = do
        x <- [ 1..50 ]
        guard $ '7' `elem` show x
        return x

    sevensOnly' :: ( Show a ) => [ a ] -> [ a ]
    sevensOnly' xs = do
        x <- xs
        guard $ '7' `elem` show x
        return x


