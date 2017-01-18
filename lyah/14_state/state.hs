
    import Control.Monad.State
    import System.Random


-- // State
{-
    newtype State' s a = State' { runState' :: s -> ( a, s ) }
    instance Monad ( State' s ) where
        return x = State' $ \ s -> ( x, s )
        ( State' h ) >>= f =
            State' $ \ s ->
                let ( a, newState ) = h s
                    ( State' g ) = f a
                in g newState
-}

--  ( >>= ) :: State s a -> ( a -> State s b ) -> State s b
--  ( >>= ) :: Maybe a -> ( a -> Maybe b ) -> Maybe b

{-
    get' = state $ \ s -> ( s, s )
    put' newState = state $ \ s -> ( ( ), newState )
-}


-- // random
--  random :: ( RandomGen g, Random a ) => g -> ( a, g )

-- // randomSt
    randomSt :: ( RandomGen g, Random a ) => State g a
    randomSt = state random


-- // Stack
    type Stack = [ Int ]

--  pop :: Stack -> ( Int, Stack )
--  pop ( x : xs ) = ( x, xs )
    pop :: State Stack Int
--  pop = state $ \ ( x : xs ) -> ( x, xs )
    pop = do
        ( x : xs ) <- get
        put xs
        return x

--  push :: Int -> Stack -> ( ( ), Stack )
--  push a xs = ( ( ), a : xs )
    push :: Int -> State Stack ( )
--  push a = state $ \ xs -> ( ( ), a : xs )
    push x = do
        xs <- get
        put ( x : xs )


    main = do

    -- // Stack
        
    --  print ( stackManip [ 5, 8, 2, 1 ] )
        print ( runState stackManip [ 5, 8, 2, 1 ] )
        print ( runState stackStuff [ 9, 0, 2, 1, 0 ] )
        print (  )
        
        print ( runState stackStuff [ 5, 0, 2, 1, 0 ] )
        print ( runState moreStack [ 100, 5, 0, 2, 1, 0 ] )
        print ( runState moreStack [ 100, 9, 0, 2, 1, 0 ] )
        print ( runState stackyState [ 1, 2, 3 ] )
        print ( runState stackyState [ 2, 2, 3 ] )
        print (  )

    -- // randomSt
        print ( runState threeCoins ( mkStdGen 33 ) )
        print (  )



--  threeCoins :: StdGen -> ( Bool, Bool, Bool )
{-
    threeCoins gen =
        let ( firstCoin, newGen ) = random gen
            ( secondCoin, newGen' ) = random newGen
            ( thirdCoin, newGen'' ) = random newGen'
        in ( firstCoin, secondCoin, thirdCoin )
-}
    threeCoins :: State StdGen ( Bool, Bool, Bool )
    threeCoins = do
        a <- randomSt
        b <- randomSt
        c <- randomSt
        return ( a, b, c )

--  stackManip :: Stack -> ( Int, Stack )
{-
    stackManip stack =
        let ( ( ), newStack1 ) = push 3 stack
            ( a, newStack2 ) = pop newStack1
            in pop newStack2
-}
    stackManip :: State Stack Int
    stackManip = do
        push 3
    --  a <- pop
        pop
        pop

    stackStuff :: State Stack ( )
    stackStuff = do
        a <- pop
        if a == 5
            then push 5
            else  do
                push 3
                push 8

    moreStack :: State Stack ( )
    moreStack = do
        a <- stackManip
        if a == 100
            then stackStuff
            else return ( )

    stackyState :: State Stack ( )
    stackyState = do
        stackNow <- get
        if stackNow == [ 1, 2, 3 ]
            then put [ 8, 3, 1 ]
            else put [ 9, 2, 1 ]


