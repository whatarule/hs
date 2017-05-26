
    main = do
        
        print ( Just 3 >>=
            ( \ x -> Just ( show x ++ "!" ) )
          )
        print ( Just 3 >>=
            ( \ x -> Just "!" >>=
                ( \ y -> Just ( show x ++ y ) )
              ) )
        print ( let x = 3; y = "!" in show x ++ y )
        print (  )
        
        print ( Nothing :: Maybe String )
        print ( ( Nothing :: Maybe String ) >>=
            ( \ x -> Just "!" >>=
                ( \ y -> Just ( show x ++ y ) )
              ) )
        print ( Just 3 >>=
            ( \ x -> ( Nothing :: Maybe String ) >>=
                ( \ y -> Just ( show x ++ y ) )
              ) )
        print ( Just 3 >>=
            ( \ x -> Just "!" >>=
                ( \ y -> ( Nothing :: Maybe String ) )
              ) )
        print (  )
        
        print ( Just 3 >>= ( \ x ->
                Just "!" >>= ( \ y ->
                Just ( show x ++ y ) ) ) )
        print ( do
            x <- Just 3
            y <- Just "!"
            Just ( show x ++ y ))
        print (  )


