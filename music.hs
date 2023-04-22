import System.Process (readProcess, readCreateProcess, readProcessWithExitCode)
import System.Exit (ExitCode, ExitCode(ExitSuccess))
import Data.Char (isSpace)
import Data.List (intercalate, dropWhile, dropWhileEnd)

clampLength :: Int -> String -> String
clampLength n string = clamped ++ (if string /= clamped then ellipses else "")
  where
    ellipses = "..."
    breakpoint = n - 4
    clamped = take breakpoint string

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


wrap pre suff string = pre ++ string ++ suff

wrapAtt :: String -> String
wrapAtt = wrap "{{" "}}"

getPlayerIcon player =
  case player of
    -- Gets unicode char for nerd font
    "chromium" -> "\62056"
    "spotify"  -> "\61884"
    "vlc"      -> "\984444"
    _            -> ""

getStatusIcon status =
  case status of
    "Playing" -> "▶️"
    "Paused"  -> "⏸️"
    _         -> ""

stripLeadingDash (' ':'-':' ':xs) = xs
stripLeadingDash x                = x

isError :: ExitCode -> Bool
isError ExitSuccess = False
isError _           = True

getPlayer        = trim <$> readProcess "playerctl" ["metadata", "-f", "{{playerName}}"] ""
getStatus player = trim <$> readProcess "playerctl" ["status", "-p", player] ""
getAttribute player att = trim <$> readProcess "playerctl" ["metadata", "-f", wrapAtt att, "-p", player] ""

main = do
  (code, statuses, err) <- readProcessWithExitCode "playerctl" ["status", "-a"] ""
  if isError code || all (== "Stopped") (lines statuses) then return () else do
    player            <- getPlayer
    status            <- trim <$> readProcess "playerctl" ["status", "-p", player] ""
    let _getAttribute =  getAttribute player
    artist            <- _getAttribute "artist"
    title             <- _getAttribute "title"
    currTime          <- _getAttribute "duration(position)"
    totTime           <- _getAttribute "duration(mpris:length)"
    putStrLn $ unwords 
      $ filter (/= mempty) 
      [ getPlayerIcon player
      , getStatusIcon status
      , if artist /= mempty then artist ++ " -" else ""
      , wrap "<fc=#ccc>" "</fc>" $ clampLength 100 title
      , currTime
      , wrap "(" ")" totTime
      ]
