
    import Control.Monad
    import Control.Monad.Writer
    import Control.Monad.State

{-
-- // liftM
--  fmap :: ( Functor f ) => ( a -> b ) -> f a -> f b
    liftM :: ( Monad m ) => ( a -> b ) -> m a -> m b
    liftM f m = m >>= ( \x -> return ( f x ) )
    liftM f m = do
        x <- m
        return $ f x

-- // ap
--  ( <*> ) :: ( Applicative f ) => f ( a -> b ) -> f a -> f b
    ap :: ( Monad m ) => m ( a -> b ) -> m a -> m b
    ap mf m = do
        f <- mf
        x <- m
        return $ f x

-- // join
    join :: ( Monad m ) => m ( m a ) -> m a
    join mm = do
        m <- mm
        m

-- // filterM
--  filter :: ( a -> Bool ) -> [ a ] -> [ a ]
    filterM :: ( Monad m ) => ( a -> m Bool ) -> [ a ] -> m [ a ]

-- // foldM
--  foldl :: ( a -> b -> a ) -> a -> [ b ] -> ai
    foldM :: ( Monad m ) => ( a ->  b -> m a ) -> a -> [ b ] -> m a

-}

  
    main = do

    -- // liftM
        print ( liftM ( * 3 ) ( Just 8 ) )
        print ( fmap ( * 3 ) ( Just 8 ) )
        print ( runWriter $ liftM not $ writer ( True, "chickpeas" ) )
        print ( runWriter $ fmap not $ writer ( True, "chickpeas" ) )
    --  print ( runState ( liftM ( + 100 ) pop ) [ 1, 2, 3, 4 ] )
    --  print ( runState ( fmap ( + 100 ) pop ) [ 1, 2, 3, 4 ] )
        print (  )

    -- // ap
        print ( ( + ) <$> Just 3 <*> Just 5 )
        print ( ( + ) <$> Just 3 <*> Nothing )
        print ( Just ( + 3 ) <*> Just 4 )
        print ( Just ( + 3 ) `ap` Just 4 )
        print ( [ ( + 1 ), ( + 2 ), ( + 3 ) ] <*> [ 10, 11 ] )
        print ( [ ( + 1 ), ( + 2 ), ( + 3 ) ] `ap` [ 10, 11 ] )
        print (  )

    -- // join
        print ( join ( Just ( Just 9 ) ) )
        print ( join ( Just ( Nothing :: Maybe Int ) ) )
        print ( join Nothing :: Maybe Int )
        print ( join [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] )
        print ( runWriter $ join ( writer ( writer ( 1, "aaa" ) , "bbb" ) ) )
        print ( join ( Right ( Right 9 ) ) :: Either String Int )
        print ( join ( Right ( Left "error" ) ) :: Either String Int  )
        print ( join $ Left "error" :: Either String Int )
    --  print ( runState ( join ( state $ /s -> ( push 10, 1 : 2 : s ) ) ) [ 0, 0, 0 ] )
        print ( joinMaybes )
        print (  )

    -- // filterM
        print ( filter ( \ x -> x < 4 ) [ 9, 1, 5, 2, 10, 3 ] ) 
        print ( fst $ runWriter $ filterM keepSmall [ 9, 1, 5, 2, 10, 3 ] )
        mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [ 9, 1, 5, 2, 10, 3 ]
        print ( powerset [ 1, 2, 3 ] )
        print (  )

    -- // foldM
        print ( foldl ( \ acc x -> acc + x ) 0 [ 2, 8, 3, 1 ] )
        print ( foldM binSmalls 0 [ 2, 8, 3, 1 ] )
        print ( foldM binSmalls 0 [ 2, 11, 3, 1 ] )
        print (  )

{- 
-}

-- // bimSmalls
    binSmalls :: Int -> Int -> Maybe Int
    binSmalls acc x
        | x > 9 = Nothing
        | otherwise = Just ( acc + x )

-- // powerset
    powerset :: [ a ] -> [ [ a ] ]
    powerset xs = filterM ( \ x -> [ True, False ] ) xs

-- // keepSmall
    keepSmall :: Int -> Writer [ String ] Bool
    keepSmall x
        | x < 4 = do
            tell [ "Keeping " ++ show x ]
            return True
        | otherwise = do
            tell [ show x ++ " is too large, throwing it away" ]
            return False

-- // joinMaybes
    joinMaybes :: Maybe Int
    joinMaybes = do
        m <- Just ( Just 8 )
        m

-- // liftA2
    liftA2 :: ( Applicative f ) => ( a -> b -> c ) -> f a -> f b -> f c
    liftA2 f x y = f <$> x <*> y

-- // Stack
    type Stack = [ Int ]

    pop :: Stack -> ( Int, Stack )
    pop ( x : xs ) = ( x, xs )

    push :: Int -> Stack -> ( ( ), Stack )
    push a xs = ( ( ), a : xs )

