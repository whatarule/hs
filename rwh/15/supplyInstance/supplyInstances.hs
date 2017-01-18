
    {-# LANGUAGE
        FlexibleInstances ,
        MultiParamTypeClasses ,
        GeneralizedNewtypeDeriving
      #-}

    import Supply hiding ( next )
    import SupplyClass hiding ( main )
    import RandomSupply hiding ( main )
    import System.Random hiding ( next )
    import Control.Monad.Reader

    newtype Reader' e a = R' { runReader' :: e -> a }
        deriving ( Functor, Applicative )

    instance Monad ( Reader' e ) where
        return a = R' $ \ _ -> a
        m >>= k = R' $ \ r -> runReader' ( k ( runReader' m r ) ) r

    ask' :: Reader' e e
    ask' = R' id


    newtype MySupply e a = MySupply { runMySupply :: Reader e a }
        deriving ( Functor, Applicative, Monad )

    instance MonadSupply e ( MySupply e ) where
        next = MySupply $ do
                  v <- ask
                  return $ Just v
    --  next = MySupply $ Just `liftM` ask

    runMS :: MySupply i a -> i -> a
    runMS = runReader . runMySupply

    main = do
    
        print $ runReader' ( ask' >>= \ x -> return ( x * 3 ) ) 2
        print $ runReader ( ask >>= \ x -> return ( x * 3 ) ) 2

        r <- ( fst . runSupply xy ) `fmap` randomsIO
        print $ r
        r <- ( fst . runSupply xy ) `fmap` randomsIO
        print $ r
        print $ runMS xy 2
        print $ ( )


    xy :: ( Num s, MonadSupply s m ) => m s
    xy = do
        Just x <- next
        Just y <- next
        return $ x * y


