
    import Data.Monoid
    import Control.Monad.Writer

    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString as S

    main = do
        
        print ( isBigGang 3 )
        print ( isBigGang 30 )
        print (  )
        
        print ( ( 3, "Smallish gang." ) `applyLog` isBigGang )
        print ( ( 30, "A freaking platoon." ) `applyLog` isBigGang )
        print (  )

        print ( ( "Tobin", "Got outlaw name." ) `applyLog` ( \x -> ( length x, "Applied length." ) ) )
        print ( ( "Bathca", "Got outlaw name." ) `applyLog` ( \x -> ( length x, "Applied length." ) ) )
        print (  )

        print ( [ 1, 2, 3 ] `mappend` [ 4, 5, 6 ] )
        print ( B.pack [ 99, 104, 105 ] `mappend` B.pack[ 104, 117, 97, 104, 117, 97 ] )
        print (  )

        print ( Sum 3 `mappend` Sum 9 )
        print ( getSum $ Sum 3 `mappend` Sum 9 )
        print (  )

        print ( addDrink "beans" )
        print ( ( "beans", Sum 10 ) `applyLog` addDrink )
        print ( ( "jerky", Sum 25 ) `applyLog` addDrink )
        print ( ( "dogmeat", Sum 5 ) `applyLog` addDrink )
        print ( ( "beans", Sum 10 ) `applyLog` addDrink `applyLog` addDrink )
        print (  )

        print ( runWriter ( return 3 :: Writer String Int ) )
        print ( runWriter ( return 3 :: Writer ( Sum Int ) Int ) )
        print ( runWriter ( return 3 :: Writer ( Product Int ) Int ) )
        print ( runWriter multWithLog )
    --  print ( runWriter $ logNumber 3 logNumber 5 )
        print (  )

        print ( runWriter $
          ( return 3 :: Writer [ String ] Int ) )
        print ( runWriter $
          ( return 3 :: Writer [ String ] Int ) >>= ( \ x ->
          writer ( x, [ "aaa" ] ) :: Writer [ String ] Int ) )
    {-
    -}
        print (  )
        
        print ( gcd' 8 3 )
        print ( runWriter $ gcd' 8 3 )
        print ( fst $ runWriter $ gcd' 8 3 )
        mapM_ putStrLn $ snd $ runWriter $ gcd' 8 3
        print (  )

{-
-- // Writer w a
    newtype Writer w a = Writer { runWriter :: ( a, w ) }
    instance ( Monoid w ) => Monad ( Writer w ) where
      return x = Writer ( x, mempty )
      ( Writer ( x, v ) ) >>= f =
        let ( Writer ( y, v' ) ) = f x
        in Writer ( y, v `mappend` v' )
-}

-- // gcd'
--  gcd' :: Int -> Int -> Int
    gcd' :: Int -> Int -> Writer [ String ] Int
    gcd' a b
      | b == 0 = do
          tell [ "Finished with " ++ show a ]
          return a
      | otherwise = do
          tell [ show a ++ " mod " ++ show b ++ " = " ++ ( show $ a `mod` b ) ]
          gcd' b ( a `mod` b )

-- // multWithLog
    logNumber :: Int -> Writer [ String ] Int
    logNumber x = writer ( x, [ "Got number: " ++ show x ] )

    multWithLog :: Writer [ String ] Int
    multWithLog = do
        a <- logNumber 3
        b <- logNumber 5
        tell [ "Gonna multiply these two" ]
    --  writer ( ( ), [ "aaa" ])
        return ( a * b )

-- // applyLog
{-
--  applyLog :: ( a, String ) -> ( a -> ( b, String ) ) -> ( b, String )
    applyLog :: ( a, [ c ] ) -> ( a -> ( b, [ c ] ) ) -> ( b, [ c ] )
    applyLog ( x, log ) f =
     let ( y, newLog ) = f x
     in ( y, log ++ newLog )
-}
    applyLog :: ( Monoid m ) => ( a, m ) -> ( a -> ( b, m ) ) -> ( b, m )
    applyLog ( x, log ) f =
     let ( y, newLog ) = f x
     in ( y, log `mappend` newLog )

-- // addDrink
    type Food = String
    type Price = Sum Int

    addDrink :: Food -> ( Food, Price )
    addDrink "beans" = ( "milk", Sum 25 )
    addDrink "jerky" = ( "whisky", Sum 99 )
    addDrink _ = ( "beer", Sum 30 )

-- // isBigGang
{-
    isBigGang :: Int -> Bool
    isBigGang x = x > 9
-}
    isBigGang :: Int -> ( Bool, String )
    isBigGang x = ( x > 9, "Compared gang size to 9." )


