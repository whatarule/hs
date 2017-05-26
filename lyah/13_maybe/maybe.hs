
    import Data.Char

    main = do

        print ( fmap ( ++ "!") ( Just "wisdom" ) )
        print ( fmap ( ++ "!" ) Nothing )
        print (  )
        
        print ( Just ( + 3 ) <*> Just 3 )
        print ( Nothing <*> Just "greed" :: Maybe String )
        return ( Nothing <*> Just "greed" :: Maybe String )
        print ( Nothing :: Maybe String )
        return Nothing
        print ( Just ord <*> Nothing )
        print (  )

        print ( max <$> Just 3 <*> Just 6 )
        print ( max <$> Just 3 <*> Nothing )
        print (  )
        
        print ( ( \x -> Just ( x + 1 ) ) 1 )
        print ( ( \x -> Just ( x + 1 ) ) 100 )
        print (  )

        print ( Just 3 `applyMaybe` \x -> Just ( x + 1 ) )
        print ( Just "smile" `applyMaybe` \x -> Just ( x ++ " :)" ) )
        print ( Nothing `applyMaybe` \x -> Just ( x + 1 )  )
        print ( Nothing `applyMaybe` \x  -> Just ( x ++ " :)" ) )
        print ( Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing )
        print ( Nothing `applyMaybe` \x -> if x > 2 then Just x else Nothing )
        print (  )
        
        print ( return "WHAT" :: Maybe String )
        print ( Just 9 >>= \x -> return ( x * 10 ) )
        print ( Nothing >>= \x -> return ( x * 10 ) )

    applyMaybe :: Maybe a -> ( a -> Maybe a ) -> Maybe a
    applyMaybe Nothing f = Nothing
    applyMaybe ( Just x ) f = f x




