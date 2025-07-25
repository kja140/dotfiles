#!/usr/bin/env sh

# === CONFIG ===
DOWNLOAD_DIR="$HOME/Videos/desktop_loop"
URL="$1"

# === CHECKS ===
if [ -z "$URL" ]; then
  echo "🚨 Usage: yt-loop-dl.sh <youtube-url>"
  exit 1
fi

mkdir -p "$DOWNLOAD_DIR"

# === DOWNLOAD ===
echo "📥 Downloading $URL ..."
yt-dlp -f 137+140 \
  --merge-output-format mp4 \
  -o "$DOWNLOAD_DIR/%(title).100s.%(ext)s" \
  "$URL"

echo "✅ Saved to $DOWNLOAD_DIR"
