
    main = do
        print ( solveRPN "10 4 3 + 2 * -" )
        print ( solveRPN "2 3.5 +" )
        print ( solveRPN "34 12 33 55 66 + * - +" )
        print ( solveRPN "90 34 12 33 55 66 + * - +" )
        print ( solveRPN "90 34 12 33 55 66 + * - + -" )
        print ( solveRPN "90 3.8 -" )
        print ( solveRPN "2.7 ln" )
        print ( solveRPN "10 10 10 10 sum 4 /" )
        print ( solveRPN "10 10 10 10 10 sum 4 /" )
        print ( solveRPN "10 2 ^" )
        print (  )


    solveRPN :: String -> Double
{-
    solveRPN expression =
        head ( foldl foldingFunction [ ] ( words expression ) )
        where foldingFunction stack item = ...
    solveRPN = head . foldl foldingFunction [ ] . words
        where foldingFunction stack item = ...
-}
    solveRPN = head . foldl foldingFunction [ ] . words
        where
          foldingFunction ( x : y : ys ) "*" = ( y * x ) : ys
          foldingFunction ( x : y : ys ) "+" = ( y + x ) : ys
          foldingFunction ( x : y : ys ) "-" = ( y - x ) : ys
          foldingFunction ( x : y : ys ) "/" = ( y / x ) : ys
          foldingFunction ( x : y : ys ) "^" = ( y ** x ) : ys
          foldingFunction ( x : xs ) "ln" = log x : xs
          foldingFunction xs "sum" = [ sum xs ] 
          foldingFunction xs numberString = read numberString : xs





