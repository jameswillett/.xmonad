import System.Process (readProcess, readCreateProcess, readProcessWithExitCode)
import System.Exit (ExitCode, ExitCode(ExitSuccess))
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)

p = readCreateProcess

trim = dropWhileEnd isSpace . dropWhile isSpace
strip = tail . init

getPlayerIcon player =
  case trim player of
    -- Gets unicode char for nerd font
    "'chromium'" -> "\62056 "
    "'spotify'"  -> "\61884 "
    "'vlc'"      -> "\984444 "
    _            -> ""

getStatusIcon status =
  case trim status of
    "Playing" -> "▶️ "
    "Paused"  -> "⏸️ "
    _         -> ""

stripLeadingDash (' ':'-':' ':xs) = xs
stripLeadingDash x            = x

isError :: ExitCode -> Bool
isError ExitSuccess = False
isError _           = True

main = do
  (code, status, err) <- readProcessWithExitCode "playerctl" ["status"] ""
  if isError code || (trim status == "Stopped") then putStrLn "" else do
    player <- readProcess "playerctl" ["metadata" , "-f",  "'{{playerName}}'"] ""
    playString <- readProcess "playerctl"
      [ "metadata", "-f"
      , "'{{artist}} - <fc=#ccc>{{title}}</fc> {{duration(position)}} ({{duration(mpris:length)}})'"
      ] ""
    putStrLn $ getPlayerIcon player ++ getStatusIcon status ++ stripLeadingDash ((strip . trim) playString)
