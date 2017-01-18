    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE FlexibleInstances #-}

    {-# LANGUAGE GADTs #-}
    {-# LANGUAGE TypeFamilies #-}

    import Control.Monad
    import qualified Control.Monad.State as S
    import qualified Control.Monad.Reader as R
    import qualified Control.Monad.Except as E
    import qualified Control.Monad.Identity as I
    import Control.Monad.Trans.Maybe
    import Control.Monad.Trans.List

    import Control.Eff
    import Control.Eff.Lift
    import Control.Eff.Exception
    import Control.Eff.Choose
    import Control.Eff.Reader.Lazy
    import Control.Eff.State.Lazy

    import Data.Void

    main = do

    -- // Eff Monad
        putStrLn "// Eff Monad"
        putStrLn ""

    -- // Transformer
        putStrLn "// Transformer"
        print $ ( ( `S.runState` 10 ) $ runMaybeT actionT :: ( Maybe Int, Int ) )
        print $ ( ( `S.runStateT` 10 ) $ actionT :: Maybe ( Int, Int ) )
        putStrLn ""

    -- // Member
        putStrLn "// Member"
        print $ ( `R.runReader` "aaa" ) $ ( `R.runReaderT` 1 ) actionRdT
        print $ run $ ( `runReader` ( 1 :: Int ) ) $ ( `runReader` "aaa" ) actionEff
        print $ run $ ( runState ( 1 :: Int ) ) $ ( runState "abc" ) actionEffSt
        print $ run $ ( runState ( 1 :: Int ) ) $ ( execState "abc" ) actionEffSt
        print $ run $ ( evalState ( 1 :: Int ) ) $ ( evalState "abc" ) actionEffSt
        putStrLn ""
        print $ run $ ( `runReader` Page 10 ) $ ( `runReader` Line 10 ) currentPos
        print $ run $ ( `runReader` Line 10 ) $ ( `runReader` Page 10 ) currentPos
        putStrLn ""

    -- // TooBigExT
        putStrLn "// TooBigExT"
        print $ I.runIdentity $ E.runExceptT $ runListT $ ex2ExT $ choiceExT [ 5, 4, 1 ]
        print $ I.runIdentity $ E.runExceptT $ runListT $ ex2ExT $ choiceExT [ 5, 7, 1 ]
        putStrLn ""

    -- // TooBig
        putStrLn "// TooBig"
        print $ run $ runErrBig $ runChoice $ exRec $ ex2 $ choose [ 5, 7, 1 ]
        print $ run $ runChoice $ runErrBig $ exRec $ ex2 $ choose [ 5, 7, 1 ]
        putStrLn ""
        print $ run $ runErrBig $ runChoice $ exRec $ ex2 $ choose [ 5, 7, 11 ]
        print $ run $ runChoice $ runErrBig $ exRec $ ex2 $ choose [ 5, 7, 11 ]
        putStrLn ""

    -- //
        putStrLn "// "
        putStrLn ""

-- // 
{-
    ioEff :: Eff ( Exc TooBig :> lift IO :> ( ) ) Int
    ioEff = do
        lift $ print $ 1
        lift $ print $ 2
        return 1
-}

-- // TooBig
    newtype TooBig = TooBig Int
        deriving ( Show, Eq, Ord )

--  instance Error TooBig

    ex2 :: ( Member ( Exc TooBig ) r ) => Eff r Int -> Eff r Int
--  ex2 :: ( Member ( Exc TooBig ) r, Member Choose r ) => Eff r Int -> Eff r Int
--  ex2 :: Eff ( Choose :> Exc TooBig :> r ) Int -> Eff ( Choose :> Exc TooBig :> r ) Int
    ex2 m = do
        v <- m
        case v of
            _ | v > 5 -> throwExc ( TooBig v )
            _ -> return v
   
    runErrBig :: Eff ( Exc TooBig :> r ) a -> Eff r ( Either TooBig a )
    runErrBig m = runExc m

    exRec :: ( Member ( Exc TooBig ) r ) => Eff r Int -> Eff r Int
    exRec m = catchExc m handler
        where handler ( TooBig n ) | n <= 7 = return n
              handler e = throwExc e 

-- // TooBigExt
    choiceExT :: ( MonadPlus m ) => [ a ] -> m a
    choiceExT = msum . map return

    ex2ExT :: E.MonadError TooBig m => m Int -> m Int
    ex2ExT m = do
        v <- m
        case v of
            _ | v > 5 -> E.throwError ( TooBig v )
            _ -> return v

-- // Member
    actionRdT :: R.ReaderT Int ( R.Reader String ) ( Int, String )
    actionRdT = do
        int <- R.asks ( * 2 )
        str <- R.lift $ R.asks reverse
        return ( int, str )

    actionEff :: ( Member ( Reader Int ) r, Member ( Reader String ) r ) => Eff r ( Int, String )
    actionEff = do
        int <- ask
        str <- ask
        return ( int :: Int, str :: String )
{-
    f :: a -> a
    f a = a

    actionEff :: ( Member ( Reader Int ) r, Member ( Reader String ) r ) => Eff r ( Int, String )
    actionEff = do
        int <- ( f :: Int -> Int ) <$> ask
        str <- ( f :: String -> String ) <$> ask
        return ( int, str )
-}
    actionEffSt :: ( Member ( State Int ) r, Member ( State String ) r ) => Eff r ( Int, String )
    actionEffSt = do
        modify ( ( * 2 ) :: Int -> Int )
        modify ( reverse :: String -> String )
        int <- get
        str <- get
        return ( int :: Int, str :: String )
{-
-}

    newtype Page = Page Int
        deriving ( Show, Eq, Ord, Num )
    newtype Line = Line Int
        deriving ( Show, Eq, Ord, Num )

    lineLen :: Int
    lineLen = 40

    linePerPage :: Int
    linePerPage = 40

    currentPos :: ( Member ( Reader Page ) r, Member ( Reader Line ) r ) => Eff r Int
    currentPos = do
        Page p <- ask
        Line l <- ask
        return $ p * linePerPage * lineLen + lineLen * l

-- // Transformer
    actionT :: ( MonadPlus m, S.MonadState Int m ) => m a
    actionT = S.modify ( + 1 ) >> mzero


