
--  module Counter where

    data Counter a = Counter {  val, 
                                step :: a }
        deriving Show
    
    type CounterOp a b = Counter a -> ( b, Counter a )

    next :: Num a => Counter a -> ( a, Counter a )
    next ( Counter v s ) =
        let v' = v + s
        in ( v', Counter v' s )

    comb :: CounterOp a b -> ( b -> CounterOp a c ) -> CounterOp a c
    comb f g = \ counter0 ->
        let ( v1, counter1 ) = f counter0
            ( v2, counter2 ) = g v1 counter1
        in  ( v2, counter2 )

    comb_ :: CounterOp a b -> CounterOp a c -> CounterOp a c
    comb_ f1 f2 = f1 `comb` \ _ -> f2

    ret :: b -> CounterOp a b
    ret x = \ counter -> ( x, counter )


    main = do
        
        print $ next $ Counter 0 1
        print $ next3 $ Counter 0 1
        print $ next3' $ Counter 0 2


    next3 = next `comb_` next `comb_` next
    next3' =  next `comb` \ v1 ->
              next `comb` \ v2 ->
              next `comb` \ v3 ->
              ret $ v1 + v2 + v3
    
