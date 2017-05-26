
    main = do
      
        print ( Left "boom" >>= \x -> return ( x + 1 ) )
        print ( Left "boom" >>= \x -> Left "no way!" :: Either String Int )
        print ( Right 100 >>= \x -> Left "no way!" :: Either String Int )
        print ( Right 3 >>= \x -> return ( x + 100 ) :: Either String Int )
        print (  )


