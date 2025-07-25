#!/bin/bash

IDLE_THRESHOLD=$((1 * 60))  # 5 minutes
LOGFILE="/tmp/idle_watch.log"

while true; do
    idle_ns=$(ioreg -c IOHIDSystem | awk '/HIDIdleTime/ { print $NF; exit }')
    idle_sec=$((idle_ns / 1000000000))

    if [ "$idle_sec" -ge "$IDLE_THRESHOLD" ]; then
        if ! pgrep -x mpv > /dev/null; then
            echo "$(date) 💤 Idle detected, launching desktop video..." >> "$LOGFILE"
            ~/start_desktop_video.sh
        fi
    fi

    sleep 10
done
