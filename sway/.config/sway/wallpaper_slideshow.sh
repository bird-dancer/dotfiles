#!/bin/sh
PID=`pidof swaybg`
while true; do
    swaybg -i $(find ~/pictures/wallpapers/ -type f | shuf -n1) -m fill &
    sleep 1
    kill $PID
    PID=`pidof swaybg`
    sleep 300
    [ "$(pidof swaybg)" != "$PID" ] && exit 0
done
