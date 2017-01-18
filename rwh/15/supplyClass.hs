
    {-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
    module SupplyClass where


    import Control.Monad
    
    import qualified Supply as S


    class ( Monad m ) => MonadSupply s m | m -> s where
        next :: m ( Maybe s )

    instance MonadSupply s ( S.Supply s ) where
        next = S.next


    main = do

        print $ S.runSupply showTwo_class [ 1, 2, 3 ]
        print $ ( )

    
    showTwo_class :: ( Show s, Monad m, MonadSupply s m ) => m String
    showTwo_class = do
        a <- next
        b <- next
        return ( "a: " ++ show a ++ ", b: " ++ show b  )

