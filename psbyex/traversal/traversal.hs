
    import Control.Monad.Identity
    import Control.Monad.State

    main = do

        putStrLn ""

    -- // Traversal
        putStrLn "// Traversal"
    {-
        class ( Functor t, Foldable t ) => Traversable t where
            traverse :: ( Applicative f ) => ( a -> f b ) -> t a -> f ( t b )
        instance Traversable [ ] where
            traverse f [ ] = pure [ ]
            traverse f ( x : xs ) = ( : ) <$> f x <*> traverse f xs
    -}
        print $ 1 : [ ]
        print $ ( : ) 1 [ ]
        print $ ( : ) <$> Just 1 <*> Just [ ]
        print $ ( + ) <$> [ 1 ] <*> [ 1..5 ]
        print $ ( + ) <$> [ 1..5 ] <*> [ 1 ]
        print $ ( + ) <$> [ 1..5 ] <*> [ 1..5 ]
        print $ ( + 1 ) <$> [ 1..5 ]
        putStrLn ""
        let l = [ ( + 1 ), ( + 2 ) ]
        let l = ( + ) <$> [ 1, 2 ]
        print $ l <*> [ 0 ] 
        let l = Just <$> [ 1, 2 ]
        print $ l
        let l = ( \ x -> [ ( x + 1 ), ( x + 2 ) ] ) <$> [ 1, 2 ]
        let l = [ ( + 1 ), ( + 2 ) ] <*> [ 1, 2 ]
        print $ l
        let l = ( \ x -> modify ( + x ) :: State Int ( ) ) <$> [ 1, 2 ]
        print $ ( `execState` 0 ) <$> l
        putStrLn ""
        print $ traverse ( \ x -> Just ( x + 1 ) ) [ 1..5 ]
        putStrLn ""
        print $ ( execState ( modify ( + 1 ) ) 0 )
        print $ ( \ x -> x + 1 ) 0
        print $ execState ( ( \ x ->  modify ( + x ) ) 1 ) 0
        print $ execState ( ( \ x ->  modify ( + x ) ) 1 ) <$> [ 0..3 ]
        putStrLn ""
        let s = ( state $ \ s -> ( ( ), s ) ) :: State Int ( )
        let s = ( state $ \ s -> ( ( ), s + 1 ) ) :: State Int ( )
        let s = do
            return ( ) :: State Int ( )
        let s = do
            modify ( + 1 ) :: State Int ( )
        let s = do
            modify ( + 1 )
            return ( ) :: State Int ( )
        let s = modify ( + 1 ) :: State Int ( )
    {-
        let s = modify [ ( + 1 ) ] :: State [ Int ] ( )
        print $ ( `execState` 0 ) $ s
        let s = do
            modify ( + 1 )
            return ( )
        let s = \ x -> ( modify ( + x ) :: State Int ( ) )
        let s = \ x -> do
            modify ( + x )
            return ( )
        print $ ( `execState` 0 ) $ ( state $ \ s -> return s )
        print $ ( `execState` 0 ) $ ( \ x -> ( state $ \ s -> return s :: State Int ( ) ) )
        print $ ( `execState` 0 ) $ ( \ x -> state $ \ s -> do
            modify ( + x )
            return ( )
            ) 1
    -}
        putStrLn ""
        let f = modify ( + 1 ) :: State Int ( )
        let f = ( \ x -> modify ( + x ) ) <$> [ 1..5 ] :: [ State Int ( ) ]
        let f = execState <$> ( ( \ x -> modify ( + x ) ) <$> [ 1..5 ] :: [ State Int ( ) ] )
        let f = execState <$> ( ( \ x -> modify ( \ sum -> sum + x ) ) <$> [ 1..5 ] :: [ State Int ( ) ] )
        print $ f <*> [ 0 ]
        let f = sequence ( ( \ x -> modify ( + x ) ) <$> [ 1..5 ] :: [ State Int ( ) ] )
        print $ execStateT f 0
        let f = runIdentity . ( execStateT $ traverse ( \ x -> modify ( + x ) :: State Int ( ) ) [ 1..5 ] )
        print $ f 0 
        let f = runIdentity . ( `execStateT` 0 ) . ( traverse ( \ x -> modify ( + x ) ) :: [ Int ] -> StateT Int Identity [ ( ) ] )
        print $ f [ 1..5 ]
        putStrLn ""
        let f = traverse ( \ x -> modify ( + x ) ) :: [ Int ] -> State Int [ ( ) ]
        print $ ( `execState` 0 ) $ do
            f [ 1..5 ]
            f [ 1..5 ]
        putStrLn ""
        traverse ( \ n -> print n >> return ( n ^ 2 ) ) [ 1..5 ]
        putStrLn ""

    -- // 
        putStrLn "// "
        putStrLn ""

-- // 

-- // 

    f = traverse ( \ x -> modify ( + x ) :: State Int ( ) ) [ 1..5 ]



