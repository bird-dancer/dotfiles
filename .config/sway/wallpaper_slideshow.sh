#!/bin/sh
while true; do
    PID=`pidof swaybg`
    swaybg -i $(find /home/felix/Pictures/wallpapers/ -type f | shuf -n1) -m fill &
    sleep 1
    kill $PID
    sleep 300
done
