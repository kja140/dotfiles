#!/bin/bash

VIDEO_DIR="$HOME/Videos/desktop_loop"
LOGFILE="/tmp/mpv_desktop.log"

# Check directory
if [ ! -d "$VIDEO_DIR" ]; then
    echo "$(date) ❌ Directory not found: $VIDEO_DIR" >> "$LOGFILE"
    exit 1
fi

# Gather files
shopt -s nullglob
FILES=("$VIDEO_DIR"/*.mp4)
if [ ${#FILES[@]} -eq 0 ]; then
    echo "$(date) ❌ No .mp4 files found in $VIDEO_DIR" >> "$LOGFILE"
    exit 1
fi
shopt -u nullglob

echo "$(date) 🎬 Launching MPV with ${#FILES[@]} video(s)" >> "$LOGFILE"

# Run MPV with forced window size, borderless, on desktop
mpv --loop-playlist=inf --no-border --ontop --geometry=100%x100%+0+0 \
    --mute=no --no-osc --no-input-default-bindings \
    --player-operation-mode=pseudo-gui \
    --title="DesktopLoop" \
    "${FILES[@]}" >> "$LOGFILE" 2>&1 &
