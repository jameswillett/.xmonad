Config { font = "xft:Bitstream Vera Sans Mono:size=10:antialias=true"
, bgColor = "black"
, alpha = 170
, fgColor = "grey"
, position = Top
, lowerOnStart = True
, commands =
    [ Run Date "%a %b %d %H:%M:%S" "date" 10
	  , Run Weather "KMTN"
      [ "--template"
      , "<station>: <skyCondition> <fc=#4682B4><tempF></fc>°F <fc=#4682B4><rh></fc>%"
      ] 36000
	  , Run Com "whoami" [] "" 0
	  , Run Com "hostname" [] "" 0
	  , Run Com "/home/james/.xmonad/wifi.sh" [] "wifi" 10
	  , Run Com "/home/james/.xmonad/volume.sh" ["-s"] "volume" 1
    , Run DynNetwork
      [ "--template" , "↑<tx>kB/s ↓<rx>kB/s"
      , "--Low"      , "1000"       -- units: B/s
      , "--High"     , "5000"       -- units: B/s
      , "--low"      , "green"
      , "--normal"   , "orange"
      , "--high"     , "red"
      ] 10

        -- cpu activity monitor
    , Run MultiCpu 
      [ "--template" , "Cpu: [<total0>, <total1>, <total2>, <total3>]%"
      , "--Low"      , "50"         -- units: %
      , "--High"     , "85"         -- units: %
      , "--low"      , "green"
      , "--normal"   , "orange"
      , "--high"     , "red"
      ] 10

        -- cpu core temperature monitor
    , Run CoreTemp
      [ "--template" , "Temp: [<core0>, <core1>, <core2>, <core3>]°C"
      , "--Low"      , "70"        -- units: °C
      , "--High"     , "80"        -- units: °C
      , "--low"      , "green"
      , "--normal"   , "orange"
      , "--high"     , "red"
      ] 50
                          
        -- memory usage monitor
    , Run Memory
      [ "--template" ,"Mem: <usedratio>%"
      , "--Low"      , "20"        -- units: %
      , "--High"     , "90"        -- units: %
      , "--low"      , "green"
      , "--normal"   , "orange"
      , "--high"     , "red"
      ] 10
    , Run StdinReader
    ]
, sepChar = "%"
, alignSep = "}{"
, template = "%whoami%@%hostname% >>= %StdinReader% %multicpu% %coretemp% %memory% }{[vol: %volume%] %dynnetwork% %wifi% | %KMTN% <fc=#49E20E>%date%</fc> "
}
