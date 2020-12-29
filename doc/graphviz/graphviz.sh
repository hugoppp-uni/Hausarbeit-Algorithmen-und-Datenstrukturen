#!/bin/bash
for filename in *.gv; do
  dot "$filename"  -T svg -o "out/${filename%.*}.svg"
done
for filename in out/*.svg; do
  rawFilename=$(basename -- "$filename")
  rsvg-convert -f pdf -o "../img/gv/${rawFilename%.*}".pdf "$filename"
done