Config 
  { font = "JetBrainsMono Nerd Font Normal 10"
  , bgColor = "#282828"
  , alpha = 255
  , fgColor = "#7CAC7A"
  , position = Top
  , lowerOnStart = True
  , commands =
    [ Run Date "%a %b %d %Y %H:%M:%S" "date" 1
    , Run Com "whoami" [] "" 0
    , Run Com "hostname" [] "" 0
    , Run Com "/home/james/.xmonad/wifi.sh" [] "wifi" 5
    , Run Com "/home/james/.xmonad/volume.sh" ["-s"] "volume" 5
    , Run Com "/home/james/.xmonad/music" [] "music" 5
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %whoami%@%hostname% >>= %StdinReader%}{%music% [vol: %volume%] | <fc=#49E20E>%date%</fc> "
  }
