#!/bin/bash

echo "🔧 Fixing broken tags in .md files..."

find content -name "*.md" -print0 | while IFS= read -r -d '' file; do
  awk '
  BEGIN { inside = 0 }
  /^\+\+\+/ { inside++; print; next }
  {
    if (inside == 1 && /^tags = \[/) {
      gsub(/""/, "\"");                         # fix double quotes
      gsub(/, *""/, "");                        # remove empty tags after commas
      gsub(/\[ *"" *,?/, "[");                  # remove empty leading tag
      gsub(/, *\]/, "]");                       # remove trailing comma before ]
      gsub(/\[, */, "[");                       # tidy up opening [
      gsub(/, +/, ", ");                        # space after comma
      gsub(/ +\]/, "]");                        # space before closing ]
    }
    print
  }
  ' "$file" > "$file.tmp" && mv "$file.tmp" "$file"
done

echo "✅ Done cleaning tags!"
