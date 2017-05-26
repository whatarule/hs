
    import Control.Monad.Reader

{-
    class ( Monad m ) => MonadReader' r m | m -> r
    where
        ask :: m r
        local :: ( r -> r ) -> m a -> m a
-}


    main = do
        print $ runReader localExample "Fred"


    localExample :: Reader String ( String, String, String )
    localExample = do
        a <- myName "First"
        b <- local ( ++ "dy" ) ( myName "Second" )
        c <- myName "Third"
        return ( a, b, c )

--  myName :: String -> IO ( String )
    myName step = do
        name <- ask
        return ( step ++ ", I am " ++ name )




