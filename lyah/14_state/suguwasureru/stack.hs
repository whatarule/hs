
--  module Stack where

    newtype Stack a = Stack [ a ]
        deriving Show

    empty :: Stack a
    empty = Stack [ ]

    pop :: Stack a -> ( a, Stack a )
    pop ( Stack ( x : xs ) ) = ( x, Stack xs )

    push :: a -> Stack a -> Stack a
    push x ( Stack xs ) = Stack ( x : xs )

-- // push'
    push' :: a -> Stack a -> ( ( ), Stack a )
    push' x ( Stack xs ) = ( ( ), Stack ( x : xs ) )

-- // empty'
    empty' :: Stack a -> ( ( ), Stack a )
    empty' _ = ( ( ), Stack [ ] )

    s = push 5 $ push 4 $ push 3 $ push 2 $ push 1 $ empty



    type StackOp a b = Stack a -> ( b, Stack a )
    newtype StackOp' a b = StackOp' { run :: Stack a -> ( b, Stack a ) }

    class MyMonad m where
        comb' :: m a -> ( a -> m b ) -> m b
        ret' :: a -> m a
        comb_' :: m a -> m b -> m b

    instance MyMonad ( StackOp' a ) where
        ret' x = StackOp' $ \ stack -> ( x, stack )
        f `comb'` g = StackOp' $ \ stack0 ->
            let ( x1, stack1 ) = run f stack0
                ( x2, stack2 ) = run ( g x1) stack1
            in  ( x2, stack2 )
        f1 `comb_'` f2 = f1 `comb'` \ _ -> f2


    main = do

    -- // Stack
        print $ s
        print (  )

    -- // sumElems
        print $ sumTwoElem s
        print $ sumFourElem s
        print (  )
        
    -- // comb0
        print $ ( comb0 pop pop ) s
        print $ pop `comb0` pop $ s
        print $ pop `comb0` pop `comb0` pop $ s
        print $ pop `comb0` pop `comb0` pop `comb0` pop $ s
        
    -- // comb1
        print $ ( pop `comb1` pop ) ( + ) $ s
        print $ ( ( pop `comb1` pop ) ( + ) `comb1` pop ) ( * ) $ s
        print (  )
        
    -- // apply
        print $ 5 `apply`
                ( + 4 ) `apply`
                ( * 3 )
        print $ 5 `apply` ( \ x ->
                x + 4 ) `apply` ( \ x ->
                x * 3 )
        print $ 5 `apply` \ x ->
                4 `apply` \ y ->
                3 `apply` \ z ->
                ( x + y ) * z
        print $ 5 `apply` \ x ->
                4 `apply` \ y ->
                3 `apply` \ z ->
                ( x + z ) * y
        print (  )

    -- // comb2
        print $ pop `comb2` ( \ x1 ->
                pop ) $ s
        print $ pop `comb2` ( \ x1 ->
                pop `comb2`   \ x2 stack ->
                ( x1 + x2, stack ) ) $ s
        print $ pop `comb2` ( \ x1 ->
                pop `comb2`   \ x2 ->
                ret ( x1 + x2 ) ) $ s
        print $ pop `comb2` ( \ x1 ->
                pop `comb2`   \ x2 ->
                pop `comb2`   \ x3 ->
                ret $ ( x1 + x3 ) * x2 ) $ s
        print (  )

    -- // topis
        print $ topis ( > 4 ) `comb2` ( \ x1 ->
                topis ( > 3 ) `comb2`   \ x2 ->
                topis ( > 2 ) `comb2`   \ x3 ->
                ret $ and [ x1, x2, x3 ] ) $ s
        print (  )
        
    -- // push'
        print $ pop `comb2` push' $ s
        print $ push' 6 `comb2_`
                pop $ s
        print $ push' 6 `comb2_`
                push' 7 `comb2_`
                push' 8 $ s
        print (  )
        
    -- // empty'
        print $ empty' $ s
        print $ push' 6 `comb2_` empty' `comb2_`
                push' 0 `comb2_` push' 1 $ s 
        print (  )

    -- // stackOp'
        print $ run ( StackOp' $ topis ( > 4 ) ) s
        print $ run poppop s
        print $ run poppush s
        print $ run poppush' s
        print $ run ( pushpush 6 7 ) s
        print $ run ( pushpop 6 ) s
        print (  )



-- // sumElems
--  sumTwoElem :: Num a => Stack a -> a
    sumTwoElem :: Num a => Stack a -> ( a, Stack a )
    sumTwoElem stack =
    --  ( fst $ pop stack ) + ( fst $ pop stack )
        let ( x1, stack1 ) = pop stack
            ( x2, stack2 ) = pop stack1
    --  in x1 + x2
        in ( x1 + x2, stack2 )
    
    sumFourElem :: ( Num a ) =>
        Stack a -> ( a, Stack a )
    sumFourElem stack =
        let ( x1, stack1 ) = pop stack
            ( x2, stack2 ) = pop stack1
            ( x3, stack3 ) = pop stack2
            ( x4, stack4 ) = pop stack3
        in ( x1 + x2 + x3 + x4, stack4 )

    sumFourElem' :: ( Num a ) => 
        ( Stack a -> ( a, Stack a ) ) ->
        Stack a -> ( a, Stack a )
    sumFourElem' f stack =
        let ( x1, stack1 ) = f stack
            ( x2, stack2 ) = f stack1
            ( x3, stack3 ) = f stack2
            ( x4, stack4 ) = f stack3
        in ( x1 + x2 + x3 + x4, stack4 )

-- // comb0
    comb0 :: ( Num a ) => 
        ( Stack a -> ( a, Stack a ) ) ->
        ( Stack a -> ( a, Stack a ) ) ->
        Stack a -> ( a, Stack a )
    comb0 f1 f2 = \ stack0 ->
        let ( x1, stack1 ) = f1 stack0
            ( x2, stack2 ) = f2 stack1
        in ( x1 + x2, stack2 )

-- // comb1
    comb1 :: 
        ( Stack a -> ( a, Stack a ) ) ->
        ( Stack a -> ( a, Stack a ) ) ->
        ( a -> a -> a ) ->
        Stack a -> ( a, Stack a )
    comb1 f1 f2 g = \ stack0 ->
        let ( x1, stack1 ) = f1 stack0
            ( x2, stack2 ) = f2 stack1
        in ( g x1 x2, stack2 )


-- // apply
    apply :: a -> ( a -> a ) -> a
    apply x f = f x

-- // comb2
    comb2 :: 
    --  ( Stack a -> ( a, Stack a ) ) ->
    --  ( a -> Stack a -> ( a, Stack a ) ) ->
    --  Stack a -> ( a, Stack a )
    --  StackOp a -> ( a -> StackOp a ) -> StackOp a
        StackOp a b -> ( b -> StackOp a c ) -> StackOp a c
    comb2 f g = \ stack0 ->
        let ( x1, stack1 ) = f stack0
            ( x2, stack2 ) = g x1 stack1
    --  let ( x1, stack1 ) = run f stack0
    --      ( x2, stack2 ) = run $ g x1 $ stack1
        in ( x2, stack2 )

    comb2_ ::
        StackOp a b -> StackOp a c -> StackOp a c
    comb2_ f g = f `comb2` \ _ -> g 

-- // ret
    ret ::
    --  a -> ( Stack a -> ( a, Stack a ) )
        b -> StackOp a b
    ret x = \ stack -> ( x, stack )

-- // topis
    topis :: ( a -> Bool ) -> Stack a -> ( Bool, Stack a )
    topis p s = let ( a, s' ) = pop s
                in ( p a, s' )


-- // poppop
    poppop :: ( Num a ) => StackOp' a a
    poppop =  StackOp' pop `comb'` \ x1 ->
              StackOp' pop `comb'` \ x2 ->
              ret' $ x1 + x2

-- // poppush
    poppush :: StackOp' a ( ) 
    poppush = StackOp' pop `comb'` \ x -> StackOp' $ push' x
    
    poppush' :: StackOp' a ( )
    poppush' = StackOp' pop `comb'` ( StackOp' . push' )

-- // pushpush
    pushpush :: a -> a -> StackOp' a ( )
    pushpush x1 x2 = StackOp' ( push' x1 ) `comb_'` ( StackOp' $ push' x2 )

-- // pushpop
    pushpop :: a -> StackOp' a a
    pushpop x = StackOp' ( push' x ) `comb_'` ( StackOp' pop )


