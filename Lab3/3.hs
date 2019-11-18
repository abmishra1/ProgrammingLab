import Data.List.Split

teams = ["BS", "CM", "CH", "CV", "CS", "DS", "EE", "HU", "MA", "ME", "PH", "ST"]

data Time = Time { hh :: Int, mm :: Int }
instance Show Time where
    show (Time hh mm) = show hh ++ ":" ++ show mm 

data Match = Empty | Match { team1 :: String, team2 :: String, date :: Int, time :: Time }
instance Show Match where
    show (Match team1 team2 date time) = team1 ++ " vs " ++ team2 ++ " " ++ show date ++ " " ++ show time

validateTime :: String -> Bool
validateTime time = do
    let (hh:mm:[]) = splitOn "." time
    hh >= "00" && hh < "24" && mm >= "00" && mm < "60"

getTime :: String -> Time
getTime timeStr = do
    let (hh:mm:[]) = map (read) (splitOn "." timeStr)
    Time { hh = hh, mm = mm }

lessThan :: Time -> Time -> Bool
lessThan (Time hh1 mm1) (Time hh2 mm2) = 
    if hh1 < hh2 then True
    else if hh1 == hh2 then mm1 < mm2
    else False
  
validateDate :: Int -> Bool
validateDate date = date >= 1 && date <= 31

genFixture :: [String] -> [Match]
genFixture [] = []
genFixture (team1:team2:remTeams) = do
    let l = length remTeams
    let d = subtract (l `div` 4) 3
    let t = if ((l `div` 2 ) `mod` 2 == 1) then Time {hh = 9, mm = 30} else Time {hh = 19, mm = 30}
    Match {
        team1 = team1,
        team2 = team2,
        date = d,
        time = t
    } : genFixture remTeams

fixture' :: String -> [Match] -> [Match]
fixture' arg allFixtures =
    filter (\(Match team1 team2 _ _) -> team1 == arg || team2 == arg) allFixtures

fixture :: String -> [Match] -> IO()
fixture arg allFixtures
    | arg == "all" = printCustom allFixtures
    | elem arg teams = printCustom (fixture' arg allFixtures)
    | otherwise = putStrLn ("Invalid argument " ++ arg)

nextMatch' :: Int -> Time -> [Match] -> [Match]
nextMatch' givenDate givenTime allFixtures
    | (null allFixtures) = []
    | otherwise = 
        if (date curMatch) > givenDate then [curMatch]
        else if (date curMatch) == givenDate && lessThan givenTime (time curMatch) then [curMatch]
        else nextMatch' givenDate givenTime nextMatches
        where (curMatch : nextMatches) = allFixtures

nextMatch :: Int -> String -> [Match] -> IO()
nextMatch givenDate givenTime allFixtures
    | not (validateDate givenDate) = putStrLn "Invalid Date"
    | not (validateTime givenTime) = putStrLn "Invalid Time"
    | otherwise = do
        printCustom (nextMatch' givenDate (getTime givenTime) allFixtures)

printCustom :: [Match] -> IO()
printCustom [] = putStrLn "No matches found"
printCustom matches = mapM_ putStrLn [show match | match <- matches]