
  import qualified Data.Map as Map

  data LockerState = Taken | Free deriving ( Show, Eq )

  type Code = String
  type LockerMap = Map.Map Int ( LockerState, Code )

  lockers :: LockerMap
  lockers = Map.fromList [
      ( 100, ( Taken, "ZD39I" ) ) ,
      ( 101, ( Free, "JAH3I" ) ) ,
      ( 103, ( Free, "IQSA9" ) ) ,
      ( 105, ( Free, "QOTSA" ) ) ,
      ( 109, ( Taken, "893JJ" ) ) ,
      ( 110, ( Taken, "99292" ) ) 
    ]

  main = do
      print ( lockerLookup 101 lockers )
      print ( lockerLookup 100 lockers )
      print ( lockerLookup 102 lockers )
      print ( lockerLookup 110 lockers )
      print ( lockerLookup 105 lockers )
      print (  )

  lockerLookup :: Int -> LockerMap -> Either String Code
  lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Just ( state, code ) ->
        if state /= Taken
          then Right code
          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
      Nothing -> 
        Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"


