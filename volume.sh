#!/bin/bash

      device=`amixer | grep -i "front right: playback"` 
      percent=`echo "$device" | sed -r 's/.*\[(.*)\%\].+/\1/'`
      dstatus=`echo "$device" | sed -r 's/.*\[(.*)\]/\1/'`
      

while getopts "udms" OPTION
do
  case $OPTION in
    u)
      volupoutput=`amixer set Master on && amixer -q set Master 4%+`
      echo "$volupoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\%\].+/expr \1 + $(( \1==100 ? 0 : 4 ))/' | sh | dbar -l 'volume ' -s x
      ;;
    d)
      voldownoutput=`amixer set Master on && amixer -q set Master 4%-`
      echo "$voldownoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\%\].+/expr \1 - $(( \1==0 ? 0 : 4 ))/' | sh | dbar -l 'volume ' -s x
      ;;
    m)
      muteoutput=`amixer set Master toggle`
      onoroff=`echo "$muteoutput" | grep -i "front right: playback" | sed -r 's/.*\[(.*)\]/\1/'`
      if [[ $onoroff == "off" ]]
      then
        echo "muted"
      else
        echo "$percent" | dbar -l 'volume ' -s x
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
