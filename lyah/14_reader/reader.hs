
    import Control.Monad.Instances

{-

-- // ( -> ) r
    instance Applicative ( ( -> ) r ) where
        pure x = _ -> x
        f <*> g = \ x -> f x $ g x
    instance Monad ( ( -> ) r ) where
        return x = _ -> x
        h >>= f = /w -> f ( h w ) w

-}


    main = do
        
        let f = ( * 5 )
        let g = ( + 3 )
        print $ ( fmap f g ) 8
        print (  )
        
        let f = ( + ) <$> ( * 2 ) <*> ( + 10 )
        print ( f 3 )
        print ( addStuff 3 )
        print (  )



    addStuff :: Int -> Int
{-
    addStuff = do
        a <- ( * 2 )
        b <- ( + 10 )
        return ( a + b )
-}
    addStuff x =
        let a = ( * 2 ) x
            b = ( + 10 ) x
        in a + b

