#!/usr/bin/env sh

#!/bin/bash

find ~/org/roam ~/org/roam/courses -type f -name "*.org" | while read -r file; do
  if awk '/^:PROPERTIES:/,/:END:/ { if ($0 ~ /^#\+hugo_section:/) exit 1 }' "$file"; then
    continue  # skip if no hugo_section inside properties
  fi

  echo "Fixing: $file"

  tmpfile=$(mktemp)
  in_properties=0
  hugo_line=""
  while IFS= read -r line; do
    if [[ $line == ":PROPERTIES:" ]]; then
      in_properties=1
      echo "$line" >> "$tmpfile"
    elif [[ $line == ":END:" ]]; then
      in_properties=0
      echo "$line" >> "$tmpfile"
      [[ -n $hugo_line ]] && echo "$hugo_line" >> "$tmpfile"
      hugo_line=""
    elif [[ $in_properties -eq 1 && $line =~ ^#\+hugo_section: ]]; then
      hugo_line="$line"  # capture for re-insert later
    else
      echo "$line" >> "$tmpfile"
    fi
  done < "$file"

  mv "$tmpfile" "$file"
done
