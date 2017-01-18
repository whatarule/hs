
    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    module Supply where

    import Control.Monad.State
    
    newtype Supply s a = S ( State [ s ] a )
      deriving ( Functor, Applicative )

    runSupply :: Supply s a -> [ s ] -> ( a, [ s ] )
    runSupply ( S m ) xs = runState m xs

    next :: Supply s ( Maybe s )
    next = S $ do
        st <- get
        case st of
            [ ] -> return Nothing
            ( x : xs ) -> do
                put xs
                return $ Just x

    unwrapS :: Supply s a -> State [ s ] a
    unwrapS ( S s ) = s

    instance Monad ( Supply s ) where
        return = S . return
        s >>= m = S ( unwrapS s >>= unwrapS . m )


{-
    main = do
        
        print $ runSupply next [ 1, 2, 3 ]
        print $ runSupply ( liftM2 ( , ) next next ) [ 1, 2, 3 ]
        print $ runSupply ( liftM2 ( , ) next next ) [ 1 ]
        print $ ( )
-}



