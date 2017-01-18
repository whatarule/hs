{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

    import Control.Monad.Trans
    import Control.Monad.Except

    import Data.Typeable
    import Control.Monad.Catch

    data MyException = MyException String
        deriving ( Show, Typeable )
    instance Exception MyException

    data MyAnotherException = MyAnotherException String
        deriving ( Show, Typeable )
    instance Exception MyAnotherException


    main = do
        putStrLn $ ""

    -- // ExceptT 
        putStrLn $ "// ExceptT"
        runExceptT $ do
            lift $ print "start"
            throwError "exception!"
            lift $ print "finish"
            `catchError` \ e -> do
                lift $ print "caught"
                lift $ print e
        putStrLn $ ""
        ret <- runExceptT $ do
            lift $ print "start"
            throwError "exception!"
            lift $ print "finish"
        print $ ret
        putStrLn $ ""
        ret <- runExceptT $ do
            lift $ print "start"
        --  file <- lift $ readFile "nothing.txt"
            throwError "exception!"
        --  lift $ print file
            return "ret value"
        print $ ret
        putStrLn $ ""

    -- // IO
        putStrLn $ "// IO"
        do
            print "start"
            file <- readFile "nothing.txt"
        --  throwError "my error"
            print file
            `catchError` \ e -> do
                print "caught"
                print e
        putStrLn $ ""

    -- // Exception 
        putStrLn $ "// Exception"
        do
            print "start"
            throwM $ MyException "exception!"
            throwM $ MyAnotherException "another exception!"
            print "finish"
            `catch` \ ( SomeException e ) -> do
                print "caught"
                print e
        putStrLn $ ""

    -- // 
        putStrLn $ "// "
        putStrLn $ ""

-- // 

-- // 

-- // 

-- // 





