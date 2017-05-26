
    {-# LANGUAGE DeriveFunctor #-}
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE TypeOperators #-}

    import Control.Eff
    import Control.Eff.Lift
    import Control.Eff.Writer.Lazy
    import Control.Eff.State.Lazy
--  import Control.Eff.Log
    
    import Data.Typeable
    import Data.Void

    import Prelude hiding ( log, ( ++ ) )

    main = do

        putStrLn ""

    -- // Log
        putStrLn "// Log"
        print $ run $ runLogger verboseAddition
        putStrLn ""
        runLift $ runIOLogger verboseAddition
        putStrLn ""
    --  print $ run $ runLogger verboseAdditionIO
        runLift $ runLogger $ verboseAdditionIO
    --  print $ runLift $ runLogger verboseAdditionLog
        putStrLn ""

    -- // LogSt
        putStrLn "// LogSt"
        print $ run $ runLoggerSt verboseAdditionSt
        putStrLn ""
        runLift $ runLoggerSt verboseAdditionStIO
        putStrLn ""

    -- // 
        putStrLn "// "
        print $ [ 1 ] ++ [ 2 ]
        putStrLn ""

-- // 

-- // LogSt
    verboseAdditionSt :: ( Member ( State [ String ] ) r ) => Eff r Int
    verboseAdditionSt = do
        modify $ ( ++ [ "I'm starting with 1..." ] )
    --  modify $ ( ( ++ [ "I'm starting with 1..." ] ) :: [ String ] -> [ String ] )
        x <- return 1
        modify $ ( ( ++ [ "and I'm adding 2..." ] ) :: [ String ] -> [ String ] )
        y <- return 2
        let r = x + y
        modify $ ( ( ++ [ "Looks like the result is " ++ show r ] ) :: [ String ] -> [ String ] )
        return r

    runLoggerSt :: Eff ( State [ String ] :> r ) a -> Eff r ( [ String ], a )
    runLoggerSt = runState ( [ ] :: [ String ] )

    verboseAdditionStIO :: ( Member ( State [ String ] ) r, SetMember Lift ( Lift IO ) r ) => Eff r Int
    verboseAdditionStIO = do
        modify $ ( ( ++ [ "I'm starting with 1..." ] ) :: [ String ] -> [ String ] )
        x <- return 1
        modify $ ( ( ++ [ "and I'm adding 2..." ] ) :: [ String ] -> [ String ] )
        y <- return 2
        let r = x + y
        modify $ ( ( ++ [ "Looks like the result is " ++ show r ] ) :: [ String ] -> [ String ] )
        log <- get
        lift $ mapM_ putStrLn ( log :: [ String ] )
        lift $ print r
        return r

    class List a where
        ( ++ ) :: a -> a -> a

    instance List ( [ a ] ) where
        [ ] ++ ys = ys
        ( x : xs ) ++ ys = x : xs ++ ys
{-
    data LogLs = Log [ String ]
        deriving ( Show )
    instance List LogLs where
        Log a ++ Log b = Log ( a ++ b )
-}
{-
        [ ] ++ [ ] = [ ]
        [ a ] ++ [ ] = [ a ]
        [ ] ++ [ b ] = [ b ]
        [ a ] ++ [ b ] = [ a, b ]
        [ a ] ++ xs = [ a ] ++ [ head xs ] ++ ( tail xs )
        xs ++ [ b ] = ( init xs ) ++ [ last xs ] ++ [ b ]
-}
{-
    addLs :: LogLs -> [ String ] -> LogLs
    ( Log a ) `addLs` b = Log ( a ++ b )

    verboseAdditionSt :: ( Member ( State LogLs ) r ) => Eff r Int
    verboseAdditionSt = do
        modify $ ( `addLs` [ "I'm starting with 1..." ] )
        x <- return 1
        modify $ ( `addLs` [ "and I'm adding 2..." ] )
        y <- return 2
        let r = x + y
        modify $ ( `addLs` [ "Looks like the result is " ++ show r ] )
        return r
-}
{-
    verboseAdditionSt :: ( Member ( State LogLs ) r ) => Eff r Int
    verboseAdditionSt = do
        modify $ ( ++ Log [ "I'm starting with 1..." ] )
        x <- return 1
        modify $ ( ++ Log [ "and I'm adding 2..." ] )
        y <- return 2
        let r = x + y
        modify $ ( ++ Log [ "Looks like the result is " ++ show r ] )
        return r

    runLoggerSt :: Eff ( State LogLs :> r ) a -> Eff r ( LogLs, a )
    runLoggerSt = runState ( Log [ ] :: LogLs )

    verboseAdditionStIO :: ( Member ( State LogLs ) r, SetMember Lift ( Lift IO ) r ) => Eff r Int
    verboseAdditionStIO = do
        modify $ ( ++ Log [ "I'm starting with 1..." ] )
        x <- return 1
        modify $ ( ++ Log [ "and I'm adding 2..." ] )
        y <- return 2
        let r = x + y
        modify $ ( ++ Log [ "Looks like the result is " ++ show r ] )
        Log log <- get
        lift $ mapM_ putStrLn log
        lift $ print r
        return r
-}

-- // Log
    type Log = Writer [ String ]

    verboseAddition :: ( Member Log r ) => Eff r Int
    verboseAddition = do
        tell [ "I'm starting with 1..." ]
        x <- return 1
        tell [ "and I'm adding 2..." ]
        y <- return 2
        let r = x + y
        tell [ "Looks like the result is " ++ show r ]
        return r

    runLogger :: Eff ( Log :> r ) a -> Eff r ( [ String ], a )
    runLogger = runWriter ( ++ ) ( [ ] :: [ String ] )

    runIOLogger :: ( SetMember Lift ( Lift IO ) r, Show a ) => Eff ( Log :> Void ) a -> Eff r ( )
    runIOLogger logAction = do
        let ( log, r ) = run $ runLogger logAction
        lift $ mapM_ putStrLn log
        lift $ print r

    verboseAdditionIO :: ( Member Log r, SetMember Lift ( Lift IO ) r ) => Eff r ( )
    verboseAdditionIO = do
        tell [ "aaa" ]
        r <- verboseAddition
        lift $ print r

    verboseAdditionLog :: ( SetMember Lift Log r, Member IO r ) => Eff r ( )
    verboseAdditionLog = do
        tell [ "aaa" ]
        return ( )

-- // 
{-
    log :: ( Member Log r ) => String -> Eff r Int
    log txt = do
        tell [ txt ]
        return 0

    verboseAddition :: ( Member Log r ) => Eff r Int
    verboseAddition = do
        log "I'm starting with 1..."
        x <- return 1
        log "and I'm adding 2..."
        y <- return 2
        let r = x + y
        log $ "Looks like the result is " ++ show r
        return r
-}


-- // 
{-
    data Log v = Log String v
        deriving ( Functor, Typeable )

    log :: ( Member Log r ) => String -> Eff r ( )
    log txt = send $ inj ( Log txt ( ) )
--  log txt = send $ \ next -> inj ( Log txt ( next ( ) ) )

    verboseAddition :: ( Member Log r ) => Eff r Int
    verboseAddition = do
        log "I'm starting with 1..."
        x <- return 1
        log "and I'm adding 2..."
        y <- return 2
        let r = x + y
        log $ "Looks like the result is " ++ show r
        return r
-}
{-
    runLogger :: Eff ( Log :> r ) a -> Eff r ( a, [ String ] )
    runLogger logAction = go logAction
        where go ( Val v ) = return ( v, [ ] )
-}


