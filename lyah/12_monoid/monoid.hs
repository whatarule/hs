
    newtype CharlList = CharList { getCharList :: [ Char ] }
                        deriving ( Eq, Show )
  --CharList :: [ Char ] -> CharList

  {-
    class Functor f where
      fmap :: ( a -> b ) -> f a -> f b
    instance Functor Maybe where
      fmap :: ( a -> b ) -> Maybe a -> Maybe b
  -}

    newtype Pair b a = Pair { getPair :: ( a, b ) }
    instance Functor ( Pair c ) where
      fmap f ( Pair ( x, y ) ) = Pair ( f x, y )

  --data CoolBool = CoolBool { getCoolBool :: Bool }
    newtype CoolBool = CoolBool { getCoolBool :: Bool }
    helloMe :: CoolBool -> String
    helloMe ( CoolBool _ ) = "hello"

  
    type IntList = [ Int ]


    main = do

        print ( CharList "this will be shown!" )
        print ( CharList "benny" == CharList "benny" )
        print ( CharList "benny" == CharList "oister" )
        print (  )
        
        print ( getPair $ fmap ( * 100 ) ( Pair ( 2, 3 ) ) )
        print ( getPair $ fmap reverse ( Pair ( "london calling", 3 ) ) )
        print (  )
        
    --  print ( undefined )
        print ( head [ 3, 4, 5, undefined, 2, undefined ] )
        print ( helloMe undefined )
        print (  )

        print ( ( [ 1, 2, 3 ] :: IntList ) ++ ( [ 1, 2, 3 ] :: [ Int ] ) )
        print (  )


