#!/bin/bash

device=`amixer | grep -i "front right: playback"` 
percent=`echo "$device" | sed -r 's/.*\[(.*)\%\].+/\1/'`
dstatus=`echo "$device" | sed -r 's/.*\[(.*)\]/\1/'`

while getopts "udms" OPTION
do
  case $OPTION in
    u)
      `pkill -f dzen`
      volupoutput=`amixer set Master on && amixer -q set Master 4%+`
      p=`echo "$volupoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\%\].+/expr \1 + $(( \1==100 ? 0 : 4 ))/' | sh`
      `dunstify -a volume -h int:value:$p --replace 42069 ""`
      ;;
    d)
      `pkill -f dzen`
      voldownoutput=`amixer set Master on && amixer -q set Master 4%-`
      p=`echo "$voldownoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\%\].+/expr \1 - $(( \1==0 ? 0 : 4 ))/' | sh`
      `dunstify -a volume -h int:value:$p --replace 42069 ""`
      ;;
    m)
      `pkill -f dzen`
      muteoutput=`amixer set Master toggle`
      onoroff=`echo "$muteoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\]/\1/'`
      if [[ $onoroff == "off" ]]
      then
        `dunstify -a volume "muted" --replace 42069`
      else
        `dunstify -a volume -h int:value:$percent --replace 42069 ""`
      fi
      ;;
    s)
      statusstring=""
      if [[ "$dstatus" == "off" ]]
      then
        statusstring=" (<fc=#ffaa00>muted</fc>)"
      fi

      if [[ "$percent" -gt 70 ]]
      then
        color="#ff0000"
      elif [[ "$percent" -gt 40 ]]
      then
        color="#ffff00"
      else
        color="#00ff00"
      fi

      echo "<fc=$color>$percent</fc>$statusstring"
      exit
      ;;
    \?)
      echo ""
      exit
      ;;
  esac
done
