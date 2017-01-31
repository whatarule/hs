
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE RankNTypes #-}

    import Control.Eff
    import Control.Eff.Lift
    import Control.Eff.Exception
    import Control.Eff.State.Lazy
    import Control.Eff.Writer.Lazy

    import qualified Control.Monad.State as S
    import qualified Control.Monad.Except as E

    import Data.Void
    import Data.Typeable

    import System.Random

    data Pierre = Pierre {
        pole :: Either Pole Pole
    ,   poleLog :: Log
    ,   story :: Story
    ,   banana :: Banana
    }

    pierre00 :: Pierre
    pierre00 = Pierre {
        pole = Right ( 0, 0 )
    ,   poleLog = [ ]
    ,   story = [ ]
    ,   banana = False
    }

    type Pole = ( Birds, Birds )
    type Birds = Int

    type PoleEff r = (
            Member ( State Pole ) r
        ,   Member ( Exc Pole ) r
        ,   Member ( Writer Log ) r
        ,   Member ( Writer Story ) r
        ,   Member ( State Banana ) r
        ) => Eff r Pole 

    type Log = [ Pole ]
    type Story = [ String ]
    type Banana = Bool

    data Side = L | R

    side :: Side -> String
    side L = "left"
    side R = "right"



-- // main
    main :: IO ( )
    main = do

        putStrLn ""
        
    -- // Pierre 
        putStrLn "// Pierre"
        runPierreEff getOnTheRoapEff
        putStrLn ""



-- // pierreEff
    type PierreEff r = (
            Member ( State Pierre ) r
        ,   SetMember Lift ( Lift IO ) r
        ) => Eff r ( ) 

    runPierreEff ::
        Eff ( State Pierre :> Lift IO :> Void ) a
        -> IO a
    runPierreEff eff = runLift $ evalState pierre00 $ eff

    getOnTheRoapEff :: PierreEff r
    getOnTheRoapEff = do
        introEff
        lift $ putStrLn "Press enter to let Pierre getting on the roap."
        cmd <- lift $ getLine
        case cmd of
            "quit" -> return ( )
            _ -> do
                lift $ do
                    putStrLn ""
                    putStrLn "---"
                    putStrLn ""
                putPoleEff $ getOnTheRoap
                lift $ putStrLn ""
                pierreEff

    introEff :: PierreEff r
    introEff = lift $ do
        putStrLn ""
        putStrLn "Pierre has decided to take a break from his job at the fish farm and try tightrope walking..."
        putStrLn "( Miran Lipovaca \"Learn You a Haskell for Great Good\" "
        putStrLn "  12.A Fistful of Monads \"Walk the line\" )"
        putStrLn ""
        putStrLn "* Pierre can \"step\", \"hop\", and \"jump\"."
        putStrLn ""

    pierreEff :: PierreEff r
    pierreEff = do
        cmd <- lift $ getLine
        case cmd of
            "step" -> stepEff
            "hop" -> hopEff
            "jump" -> jumpEff
            _ -> do
                lift $ putStrLn "Pierre can't do that action!"
                pierreContEff

    stepEff :: PierreEff r
    stepEff = do
        pierre <- get
        putPoleEff $ tell [ "Taking a step..." ]
        gen0 <- lift $ newStdGen
        let ( n, _ ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
        case n of
            _ | n < 10 -> lostStepEff
            _ -> do
                case pierre of
                    _ | banana pierre -> bananaEffPr
                    _ -> do
                        gen0 <- lift $ newStdGen
                        let ( n, gen1 ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
                        case n of
                            _ | n < 70 -> do
                                landEffPr
                                checkBalanceEffPr
                            _ -> bananaEffPr

    hopEff :: PierreEff r
    hopEff = do
        pierre <- get
        putPoleEff $ tell [ "Made a hop!" ]
        gen0 <- lift $ newStdGen
        let ( n, _ ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
        case n of
            _ | n < 20 -> lostStepEff 
            _  -> do
                case pierre of 
                    _ | banana pierre -> do
                        putPoleEff $ do
                            tell ( [ "Hopping over a yellow-yellow-banana skin, yeah!" ] :: Story )
                        put $ pierre { banana = False }
                    _ -> return ( ) 
                flyawayEffPr
                checkBalanceEffPr
    
    jumpEff :: PierreEff r
    jumpEff = do
        putPoleEff $ tell [ "Jumping from the roap!" ]
        gen0 <- lift $ newStdGen
        let ( n, _ ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
        case n of
            _ | n < 70 -> pfLandEffPr
            _ -> knLandEffPr
        groundEff

    lostStepEff :: PierreEff r
    lostStepEff = do
        putPoleEff $ do
            tell [ "Lost his step on the roap!" ]
        hpLandEffPr

    pfLandEffPr :: PierreEff r
    pfLandEffPr = do
        putPoleEff $ pfLandEff
        lift $ do
            putStrLn ""
            putStrLn "Press enter..." 
            getLine
        groundEff
        
    knLandEffPr :: PierreEff r
    knLandEffPr = do
        putPoleEff $ knLandEff
        lift $ do
            putStrLn ""
            putStrLn "Press enter..." 
            getLine
        flyawayEffPr
        flyawayEffPr
        putPoleEff $ do
            r <- get
            throwExc ( r :: Pole )
        lift $ putStrLn ""
        groundEff
        
    hpLandEffPr :: PierreEff r
    hpLandEffPr = do
        putPoleEff $ hpLandEff
        lift $ do
            putStrLn ""
            putStrLn "Press enter..." 
            getLine
        flyawayEffPrAll
        putPoleEff $ do
            r <- get
            throwExc ( r :: Pole )
        lift $ putStrLn ""
        groundEff


    landEffPr :: PierreEff r
    landEffPr = do
        gen0 <- lift $ newStdGen
        let ( sd, gen1 ) = randomSide gen0 :: ( Side, StdGen )
            ( n, gen2 ) = randomR ( 1, 3 ) gen1 :: ( Int, StdGen )
        putPoleEff $ landEff sd n 

    randomSide :: StdGen -> ( Side, StdGen )
    randomSide gen =
        let ( n, newGen ) = random gen :: ( Int, StdGen )
        in case n `mod` 2 of
            0 -> ( R, newGen )
            1 -> ( L, newGen )

    checkBalanceEffPr :: PierreEff r
    checkBalanceEffPr = do
        pierre <- get
        let Right r = pole pierre
            ( left, right ) = r :: Pole
            ab = abs $ left - right
        case ab of
            _ | ab < 4 -> do
                putPoleEff $ do
                    tell ( [ "Balanced in " ++ show r ++ "..."] :: Story )
                pierreContEff
            _ -> do
                putPoleEff $ do
                    tell ( [ "Unbalanced in " ++ show r ++ "!"] :: Story )
                gen0 <- lift $ newStdGen
                let ( n, gen1 ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
                case n of
                    _ | n < 10 -> pfLandEffPr
                    _ | n < 70 -> knLandEffPr
                    _ -> hpLandEffPr

    flyawayEffPr :: PierreEff r
    flyawayEffPr = do
        gen0 <- lift $ newStdGen
        let ( sd, gen1 ) = randomSide gen0 :: ( Side, StdGen )
            ( n, gen2 ) = randomR ( 1, 2 ) gen1 :: ( Int, StdGen )
        putPoleEff $ flyawayEff sd n

    flyawayEffPrAll :: PierreEff r
    flyawayEffPrAll = do
        pierre <- get
        let Left ( left, right ) = pole pierre
        putPoleEff $ flyawayEff L left
        putPoleEff $ flyawayEff R right

    bananaEffPr :: PierreEff r 
    bananaEffPr = do
        pierre <- get
        case pierre of
            _ | banana pierre -> do
                gen0 <- lift $ newStdGen
                let ( n, gen1 ) = randomR ( 0, 99 ) gen0 :: ( Int, StdGen )
                case n of
                    _ | n < 70 -> do
                        putPoleEff $ do
                            tell ( [ "Slipped on a banana skin!" ] :: Story )
                        hpLandEffPr    
                    _ -> do
                        putPoleEff $ do
                            tell [ "Stepping through a banana skin, yes!" ]
                        put $ pierre { banana = False }
                        pierreContEff
            _ -> do 
                put $ pierre { banana = True }
                putPoleEff $ do
                    tell [ "Something yellow on the roap.." ]
                pierreContEff


    putPoleEff ::
        Eff ( State Pole :> Exc Pole :> Writer Log :> Writer Story :> State Banana :> Void ) a
        -> PierreEff r
    putPoleEff eff = do
        pierre <- get
        let pierreNew = ( `orderedWr` pierre ) $ runPoleEff pierre eff
        put $ pierreNew
        lift $ printPierreStoryN pierreNew pierre

    orderedWr :: Pierre -> Pierre -> Pierre
    orderedWr pierre pierreOld =
        ( `orderedStory` pierreOld ) $
        ( `orderedLog` pierreOld ) $ pierre 
        
    orderedLog:: Pierre -> Pierre -> Pierre
    orderedLog pierre pierreOld =
        let w = poleLog pierre
            wOld = poleLog pierreOld
            lenO = length $ wOld
            wNew = take ( length w - lenO ) w
        in pierre { poleLog = wOld ++ wNew }

    orderedStory :: Pierre -> Pierre -> Pierre
    orderedStory pierre pierreOld =
        let w = story pierre
            wOld = story pierreOld
            lenO = length $ wOld
            wNew = take ( length w - lenO ) w
        in pierre { story = wOld ++ wNew }


    pierreContEff :: PierreEff r
    pierreContEff = do
        lift $ putStrLn ""
        pierre <- get
        case pole pierre of
            Right _ -> pierreEff
            Left _ -> groundEff

    groundEff :: PierreEff r
    groundEff = do
        pierre <- get
        lift $ do
            putStrLn "Press enter to show the log and result."
            getLine
            putStrLn "log & result :"
            printPierreLog pierre
            printPierrePole pierre
            putStrLn ""
            putStrLn "Press enter to show the Pierre's whole story."
            getLine
            putStrLn "whole story :"
            printPierreStory pierre
            putStrLn ""

    
-- // PoleEff
    printPoleEff ::
        Eff ( State Pole :> Exc Pole :> Writer Log :> Writer Story :> State Banana :> Void ) a -> IO ( )
    printPoleEff p = do
        let pierre = runPoleEff pierre00 p
        printPierre pierre

    printPierre :: Pierre -> IO ( )
    printPierre pierre = do
        printPierreStory pierre
        printPierreLog pierre
        printPierrePole pierre
        putStrLn ""

    printPierrePole :: Pierre -> IO ( )
    printPierrePole pierre = do
        print $ pole pierre

    printPierreLog :: Pierre -> IO ( )
    printPierreLog pierre = do
        mapM_ print $ poleLog pierre

    printPierreStory :: Pierre -> IO ( )
    printPierreStory pierre = do
        mapM_ putStrLn $ story pierre

    printPierreStoryN :: Pierre -> Pierre -> IO ( )
    printPierreStoryN pierre pierreOld = do
        let s = story pierre
            lenO = length $ story pierreOld
            storyNew = drop lenO s
        --  storyNew = take ( length s - lenO ) s
        mapM_ putStrLn $ storyNew
   

    runPoleEff ::
        Pierre ->
        Eff ( State Pole :> Exc Pole :> Writer Log :> Writer Story :> State Banana :> Void ) a -> Pierre
    runPoleEff pierre eff =
        let r0 = run $ runPoleEffStB pierre $ runPoleEffWrS pierre $ runPoleEffWrL pierre $ runPoleEffExc $ execPoleEffStP pierre eff
            ( stB, r1 ) = r0
            ( wrS, r2 ) = r1
            ( wrL, excP ) = r2
        in Pierre excP wrL wrS stB

    execPoleEffStP :: Pierre -> Eff ( State Pole :> r ) a -> Eff r Pole
    execPoleEffStP pierre m = execState p m
        where p = case pole pierre of Right pR -> pR :: Pole
                                      Left pL -> pL :: Pole

    runPoleEffExc :: Eff ( Exc Pole :> r ) a -> Eff r ( Either Pole a ) 
    runPoleEffExc m = runExc m

    runPoleEffWrL :: Pierre -> Eff ( Writer Log :> r ) a -> Eff r ( Log, a )
    runPoleEffWrL pierre m = runWriter ( ++ ) wL m
        where wL = poleLog pierre :: Log

    runPoleEffWrS :: Pierre -> Eff ( Writer Story :> r ) a -> Eff r ( Story, a )
    runPoleEffWrS pierre m = runWriter addR wS m
        where wS = story pierre :: Story
              addR xs ys = ( ++ ) ( xs ) ( ys )

    runPoleEffStB :: Pierre -> Eff ( State Banana :> r ) a -> Eff r ( Banana, a )
    runPoleEffStB pierre m = runState b m
        where b = banana pierre :: Banana


    getOnTheRoap :: PoleEff r
    getOnTheRoap = do
        r <- get
        tell ( [ "Pierre is getting on the roap with a long pole..." ] :: Story )
        checkBalanceEff
        return ( r :: Pole )

    landEffCh :: Side -> Birds -> PoleEff r
    landEffCh sd n = do
        landEff sd n
        checkBalanceEff

    flyawayEffCh :: Side -> Birds -> PoleEff r
    flyawayEffCh sd n = do
        flyawayEff sd n
        checkBalanceEff

    landEff :: Side -> Birds -> PoleEff r
    landEff sd n = do
        modify ( land sd n )
        let br | n == 1 = "A bird is "
               | otherwise = show n ++ " birds are "
        tell ( [ br ++ "landing on the " ++ side sd ++ " side of the pole..." ] :: Story )
        r <- get
        tell ( [ r ] :: Log ) 
        return r

    flyawayEff :: Side -> Birds -> PoleEff r
    flyawayEff sd 0 = do
        p <- get
        tell ( [ "No birds on the " ++ side sd ++ " side of the pole..." ] :: Story )
        return p        
    flyawayEff sd n = do
        p <- get
        case flyawayEi sd n p of
            Right _ -> do
                modify ( flyaway sd n )
                let br | n == 1 = "A bird is "
                       | otherwise = show n  ++ " birds are "
                tell ( [ br ++ "flying away from the " ++ side sd  ++ " side of the pole..." ] :: Story )
                r <- get
                tell ( [ r ] :: Log ) 
                return r
            Left _ -> flyawayEff sd ( n - 1 )

    checkBalanceEff :: PoleEff r
    checkBalanceEff = do
        r <- get
        let ( left, right ) = r :: Pole 
            ab = abs $ left - right
        case ab of
            _ | ab < 4 -> do
                tell ( [ "Balanced in " ++ show r ++ "..."] :: Story )
                return r
            _ -> do
                tell ( [ "Unbalanced in " ++ show r ++ "!"] :: Story )
                pfLandEff

    bananaEff :: PoleEff r
    bananaEff = do
        tell ( [ "Slipped on a banana skin!" ] :: Story )
        hpLandEff

    pfLandEff :: PoleEff r
    pfLandEff = do
        tell ( [ "Pierre made a perfect landing on the ground." ] :: Story )
        r <- get
        throwExc ( r :: Pole )
        
    knLandEff :: PoleEff r
    knLandEff = do
        tell ( [ "Pierre made a landing on his knee." ] :: Story )
        r <- get
        throwExc ( r :: Pole )
        
    hpLandEff :: PoleEff r
    hpLandEff = do
        tell ( [ "Pierre made a hip-landing on the muddy ground!!" ] :: Story )
        r <- get
        throwExc ( r :: Pole )
        
-- // Pole
    landLeft :: Birds -> Pole -> Pole
    landLeft n ( left, right ) = ( left + n, right )

    landRight :: Birds -> Pole -> Pole
    landRight n ( left, right ) = ( left, right + n )

    ( -: ) :: a -> ( a -> b ) -> b
    x -: f = f x

    land :: Side -> Birds -> Pole -> Pole
    land sd n ( left, right ) = case sd of
        L -> ( left + n, right )
        R -> ( left, right + n )

    flyaway :: Side -> Birds -> Pole -> Pole
    flyaway sd n ( left, right ) = case sd of
        L -> ( left - n, right )
        R -> ( left, right - n )

-- // PoleEi
    type PoleEi = Either Pole Pole
    
    landEi :: Side -> Birds -> Pole -> PoleEi
    landEi sd n p = do
        let r = land sd n p
            ( left, right ) = r
            ab = abs $ left - right
        case ab of
            _ | ab < 4 -> Right r
            _ -> Left r

    flyawayEi :: Side -> Birds -> Pole -> PoleEi
    flyawayEi sd n p = do
        let r = flyaway sd n p
            ( left, right ) = r
            sum = left + right
            prd = left * right
        case r of
            _ | sum >= 0 && prd >= 0 -> Right r
            _ -> Left r
        --  _ -> flyawayEi sd ( n - 1 ) p

    bananaEi :: Pole -> PoleEi
    bananaEi p = Left p


