{-
    Problem: Schedule and query fixtures.
    Convention: functionName' denotes a helper function and functionName is its driver function
        Driver runs validations, supplies additional arguments and helper does the main recursion.
    Author: Nitin Kedia, Abhinav Mishra
-}

import Data.IORef
import System.Random
import System.IO.Unsafe
import Data.List.Split

teams = ["BS", "CM", "CH", "CV", "CS", "DS", "EE", "HU", "MA", "ME", "PH", "ST"]

-- Custom data type for Time
-- contains two integers for hour and minute in 24-hr format
-- Override its default print function 
data Time = Time { hh :: Int, mm :: Int }
instance Show Time where
    show (Time hh mm)
        | hh == 0 = "12" ++ ":" ++ show mm ++ " AM"
        | hh < 12 = show hh ++ ":" ++ show mm ++ " AM"
        | hh == 12 = "12" ++ ":" ++ show mm ++ " PM"
        | otherwise = show (hh - 12) ++ ":" ++ show mm ++ " PM"

-- Similarly, define a "Match" type denoting a fixture, override to beautify print    
data Match = Empty | Match { team1 :: String, team2 :: String, date :: Int, time :: Time }
instance Show Match where
    show (Match team1 team2 date time) = team1 ++ " vs " ++ team2 ++ " " ++ show date ++ "-11 " ++ show time

-- Mutable Global variable to store the list of fixtures using IORef, it read and write functions
allFixtures :: IORef [Match]
{-# NOINLINE allFixtures #-}
allFixtures = unsafePerformIO (newIORef [])

getAllFixtures :: IO [Match]
getAllFixtures = do readIORef allFixtures
setAllFixtures :: [Match] -> IO()
setAllFixtures newFixtures = do writeIORef allFixtures newFixtures

{-
    Utility functions for time:
        1. Validate time string must be of format "hh.mm"
        2. getTime: Convert time string to custom Time Type
        3. lessThan: returns if first parameter is smaller 
        4. Validate date must be from (Nov) 1 to 30    
-}
validateTime :: [String] -> Bool
validateTime (hh:mm:[]) = hh >= "00" && hh < "24" && mm >= "00" && mm < "60"
validateTime _ = False

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
validateDate date = date >= 1 && date <= 30

{-
    Generate a list of fixtures from a random permuation of teams array. Steps:
        1. Shuffle the default order.
        2. Keep on taking two teams from the head at a time and make a fixture between them.
        we have worked out a direct mapping from index of current pair to fixture date and time. Eg.
         INDEX DATE TIME
         0     1    9:30AM
         1     1    7:30PM
         2     2    9:30AM
         3     2    7:30PM .. so on
         i.e. let  l = length of remaining list, it gives us the index of current pair 
         date = 3 - (length of remaining list) % 4
         time = whether number of remaining matches is odd or even
         3. Save the fixture list for future queries.
-}
genFixture' :: [String] -> [Match]
genFixture' [] = []
genFixture' (team1:team2:remTeams) = do
    let l = length remTeams
    let d = subtract (l `div` 4) 3
    let t = if ((l `div` 2 ) `mod` 2 == 1) then Time {hh = 9, mm = 30} else Time {hh = 19, mm = 30}
    Match {
        team1 = team1,
        team2 = team2,
        date = d,
        time = t
    } : genFixture' remTeams

genFixture :: IO()
genFixture = do
    steams <- shuffle teams
    setAllFixtures (genFixture' steams)

-- shuffle subroutine to generate a random permuation of the default order
shuffle :: [String] -> IO [String]
shuffle teams = 
    if (length teams < 2) then return teams 
    else do
        i <- System.Random.randomRIO (0, length(teams)-1)
        r <- shuffle (take i teams ++ drop (i+1) teams)
        return (teams!!i : r)

{-
    Get a team's fixture by iterating over the fixtures list and see if that team is invloved.
    Get all fixtures, pretty print all fixtures
    Validations: arg can be  "all", initials of existing team like "CS", otherwise invalid
-}
fixture' :: String -> [Match] -> [Match]
fixture' arg allFixtures =
    filter (\(Match team1 team2 _ _) -> team1 == arg || team2 == arg) allFixtures

fixture :: String -> IO()
fixture arg
    | arg == "all" = do
        allFixtures <- getAllFixtures
        printCustom allFixtures
    | elem arg teams = do
        allFixtures <- getAllFixtures
        printCustom (fixture' arg allFixtures)
    | otherwise = putStrLn ("No team exists with name " ++ arg)

{-
    Get next match from given date and time by iterating over fixtures and finding the first match
    with a later date / same date and a later time. Iteration happens recursively.
    Validations: Date in [1,30], time in [00:00, 23:59].
-}
nextMatch' :: Int -> Time -> [Match] -> [Match]
nextMatch' givenDate givenTime allFixtures
    | (null allFixtures) = []
    | otherwise = 
        if (date curMatch) > givenDate then [curMatch]
        else if (date curMatch) == givenDate && lessThan givenTime (time curMatch) then [curMatch]
        else nextMatch' givenDate givenTime nextMatches
        where (curMatch : nextMatches) = allFixtures

nextMatch :: Int -> String -> IO()
nextMatch givenDate givenTime
    | not (validateDate givenDate) = putStrLn "Invalid Date, must be from 1 to 30."
    | not (validateTime (splitOn "." givenTime)) = putStrLn "Invalid Time, use 24-hour format."
    | otherwise = do
        allFixtures <- getAllFixtures
        printCustom (nextMatch' givenDate (getTime givenTime) allFixtures)

-- Utility of print a list of fixtures (if any) each in a new line
printCustom :: [Match] -> IO()
printCustom [] = putStrLn "No matches found"
printCustom matches = mapM_ putStrLn [show match | match <- matches]