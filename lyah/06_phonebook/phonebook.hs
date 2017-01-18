
    phoneBook = [
        ( "betty", "555-2938" ),
        ( "bonnie", "452-2928" ),
        ( "pasty", "493-2928" ),
        ( "lucille", "205-2928" ),
        ( "wendy", "939-8282" ),
        ( "penny", "853-2492" )
      ]
{-
    phoneBook = [ ( "penny", "853-2492" ), ( "", "" ) ]
-}
    
    main = do
        print ( findKey "penny" phoneBook )
        print ( findKey "betty" phoneBook )
        print ( findKey "wilma" phoneBook )
        print (  )

{-
    findKey :: ( Eq k ) => k -> [ ( k, v ) ] -> v
    findKey key xs = snd . head . filter ( \x -> key == k ) $ xs
-}
{-
    findKey :: ( Eq k ) => k -> [ ( k, v ) ] -> Maybe v
    findKey key [ ] = Nothing
    findKey key ( ( k, v ) : xs )
      | key == k = Just v
      | otherwise = findKey key xs
-}
    findKey :: ( Eq k ) => k -> [ ( k, v ) ] -> Maybe v
    findKey key xs = foldr
                      ( \ ( k, v ) acc -> if key == k then Just v else acc )
                      Nothing xs



