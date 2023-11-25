import System.Process (readProcess, readCreateProcess, readProcessWithExitCode)
import System.Exit (ExitCode, ExitCode(ExitSuccess))
import Utils (clampLength, trim, wrap, wrapAtt, wrapColor)

getPlayerIcon player =
  case player of
    -- Gets unicode char for nerd font
    "chromium" -> wrapColor "DB4437" "\62056"
    "spotify"  -> wrapColor "1DB954" "\61884"
    "vlc"      -> wrapColor "E47B00" "\984444"
    _            -> ""

getStatusIcon status =
  case status of
    "Playing" -> "\61515"
    "Paused"  -> "\61516"
    _         -> ""

isError :: ExitCode -> Bool
isError = (/=) ExitSuccess

choose a b
  | null a    = b
  | otherwise = a

getPlayerAndStatus statuses  = do
  players <- lines <$> readProcess "playerctl" ["metadata", "-f", "{{playerName}}", "-a"] ""
  let playersWithStatuses = zip (filter (/= "Stopped") statuses) players
  let pausedPlayers = filter ((=="Paused") . fst) playersWithStatuses
  let playingPlayers = filter ((=="Playing") . fst) playersWithStatuses
  return $ head $ choose playingPlayers pausedPlayers

getAttribute player att = trim <$> readProcess "playerctl" ["metadata", "-f", wrapAtt att, "-p", player] ""

main = do
  (code, statuses, err) <- readProcessWithExitCode "playerctl" ["status", "-a"] ""
  if isError code || all (== "Stopped") (lines statuses) then return () else do
    (status, player)  <- getPlayerAndStatus (lines statuses)
    let _getAttribute =  getAttribute player
    artist            <- _getAttribute "artist"
    title             <- _getAttribute "title"
    currTime          <- _getAttribute "duration(position)"
    totTime           <- _getAttribute "duration(mpris:length)"
    putStrLn $ unwords
      $ filter (/= mempty)
        [ getPlayerIcon player
        , getStatusIcon status
        , if artist /= mempty then wrapColor "aaa" artist ++ " -" else ""
        , wrapColor "ccc" $ clampLength 100 title
        , currTime
        , wrap "(" ")" totTime
        ]
