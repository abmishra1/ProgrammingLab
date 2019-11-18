
teams = ["BS", "CM", "CH", "CV", "CS", "DS", "EE", "HU", "MA", "ME", "PH", "ST"]

data Match = Empty | Match { team1 :: String, team2 :: String, date :: Int, time :: String } deriving (Show)
-- validateTime :: String -> Bool
-- validateTime time =
    


genFixture :: [String] -> [Match]
genFixture [] = []
genFixture (team1:team2:remTeams) = do
    let l = length remTeams
    let d = subtract (l `div` 4) 3
    let t = if ((l `div` 2 ) `mod` 2 == 1) then "9:30" else "7:30"
    Match {
        team1 = team1,
        team2 = team2,
        date = d,
        time = t
    } : genFixture remTeams


fixture :: String -> [Match] -> [Match]
fixture arg allFixtures
    | arg == "all" = allFixtures
    | otherwise = filter (\(Match team1 team2 _ _) -> team1 == arg || team2 == arg) allFixtures

code :: String -> String
code "9:30" = "09:30"
code "7:30" = "19.30"

nextMatch :: Int -> String -> [Match] -> Match
nextMatch date time [] = Empty
nextMatch d t (curMatch : nextMatches) = if (date curMatch) > d then curMatch
                                            else if (date curMatch) == d && t < (code (time curMatch)) then curMatch
                                            else nextMatch d t nextMatches

    -- 