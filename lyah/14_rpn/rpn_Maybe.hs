
    import Control.Monad 

    main = do

        print ( readMaybe "1" :: Maybe Int )
        print ( readMaybe "GO TO HELL" :: Maybe Int )
        print (  )
        
        print ( foldingFunction [ 3, 2 ] "*" )
        print ( foldingFunction [ 3, 2 ] "-" )
        print ( foldingFunction [ ] "*" )
        print ( foldingFunction [ ] "1" )
        print ( foldingFunction [ ] "1 wawawawa" )
        print (  )

        print ( solveRPN "1 2 * 4 +" )
        print ( solveRPN "1 2 * 4 + 5 *" )
        print ( solveRPN "1 2 * 4" )
        print ( solveRPN "1 8 wharglbllargh" )
        print (  )


    solveRPN :: String -> Maybe Double
    solveRPN st = do
        [ result ] <- foldM foldingFunction [ ] ( words st )
        return result

    readMaybe :: ( Read a ) => String -> Maybe a
    readMaybe st = case reads st of
                        [ ( x, "" ) ] -> Just x
                        _ -> Nothing

    foldingFunction :: [ Double ] -> String -> Maybe [ Double ]
    foldingFunction ( x : y : ys ) "*" = return $ ( y * x ) : ys
    foldingFunction ( x : y : ys ) "+" = return $ ( y + x ) : ys
    foldingFunction ( x : y : ys ) "-" = return $ ( y - x ) : ys
    foldingFunction ( x : y : ys ) "/" = return $ ( y / x ) : ys
    foldingFunction ( x : y : ys ) "^" = return $ ( y ** x ) : ys
    foldingFunction ( x : xs ) "ln" = return $ log x : xs
    foldingFunction xs "sum" = return $ [ sum xs ] 
    foldingFunction xs numberString = liftM ( : xs ) $ readMaybe numberString





