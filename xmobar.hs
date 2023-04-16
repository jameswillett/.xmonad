Config { font = "JetBrainsMono Nerd Font Normal 12"
, bgColor = "#282828"
, alpha = 255
, fgColor = "#7CAC7A"
-- will probably need to change this in a branch for each computer
-- or figure out a more dynamic solution
-- currently very specific to my desktop
, position = Static { xpos = 1100 , ypos = 20 , width = 3400 , height = 30 }
, lowerOnStart = True
, commands =
    [ Run Date "%a %b %d %H:%M:%S" "date" 10
    , Run Com "whoami" [] "" 0
    , Run Com "hostname" [] "" 0
    , Run Com "/home/james/.xmonad/wifi.sh" [] "wifi" 10
    , Run Com "/home/james/.xmonad/volume.sh" ["-s"] "volume" 1
    , Run StdinReader
    ]
, sepChar = "%"
, alignSep = "}{"
, template = " %whoami%@%hostname% >>= %StdinReader%}{[vol: %volume%] | <fc=#49E20E>%date%</fc> "
}
