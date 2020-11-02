#!/bin/sh

iwconfig wlan0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

connected_devices=`nmcli dev wifi | grep "^*"`
essid=`echo "$connected_devices" | awk -F ' {2,}' '{print $3}'`
str=`echo "$connected_devices" | awk -F ' {2,}' '{print $7}'`
bars=`echo "$connected_devices" | awk -F ' {2,}' '{print $8}'`
# echo $essid $bar
echo ["$essid"] "$bars"

exit 0
