
    import Data.Ratio
    import Data.List ( all )

    newtype Prob a = Prob { getProb :: [ ( a, Rational ) ] }
                            deriving Show

    instance Functor Prob where
      fmap f ( Prob xs ) = Prob $ map ( \ ( x, p ) -> ( f x, p ) ) xs

    flatten :: Prob ( Prob a ) -> Prob a
    flatten ( Prob xs ) = Prob $ concat $ map multAll xs
      where multAll ( Prob innerxs, p ) = map ( \ ( x, r ) -> ( x, r * p ) ) innerxs

    instance Applicative Prob where
      pure f = Prob [ ( f, 1 % 1 ) ]
      Prob [ ( f, p ) ] <*> something = fmap f something

    instance Monad Prob where
      return x = Prob [ ( x, 1 % 1 ) ]
      m >>= f = flatten ( fmap f m )
      fail _ = Prob [ ]


    thisSituation :: Prob ( Prob Char )
    thisSituation = Prob [
      ( Prob [ ( 'a', 1 % 2 ), ( 'b', 1 % 2 ) ], 1 % 4 ) ,
      ( Prob [ ( 'c', 1 % 2 ), ( 'd', 1 % 2 ) ], 3 % 4 ) ]

    data Coin = Heads | Tails
                deriving ( Show, Eq )
    
    coin :: Prob Coin
    coin = Prob [ ( Heads, 1 % 2 ), ( Tails, 1 % 2 ) ]

    loadedCoin :: Prob Coin
    loadedCoin = Prob [ ( Heads, 1 % 10 ), ( Tails, 9 % 10 ) ]

    flipThree :: Prob Bool
    flipThree = do
        a <- coin
        b <- coin
        c <- loadedCoin
        return ( all ( == Tails ) [ a, b, c ] )

    main = do
    
    -- Data.Ratio
        print ( [ ( 3, 0.5 ), ( 5, 0.25 ), ( 9, 0.25 ) ] )
        print ( [ ( 3, 1 / 2 ), ( 5, 1 / 4 ), ( 9, 1 / 4 ) ] )
        print ( 1 / 4 )
        print ( 1 % 4 )
        print ( 1 % 2 + 1 % 2 )
        print ( 1 % 3 + 5 % 4 )
        print ( [ ( 3, 1 % 2 ), ( 5, 1 % 4 ), ( 9, 1 % 4 ) ] )
        print (  )

    -- Prob
        print ( fmap negate ( Prob [ ( 3, 1 % 2 ), ( 5, 1 % 4 ), ( 9, 1 % 4 ) ] ) )
        print ( flatten thisSituation )
        print ( getProb flipThree )
        print (  )


