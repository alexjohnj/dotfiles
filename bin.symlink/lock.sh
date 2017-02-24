#!/usr/bin/env sh
# 
# Based on savoca/dotfiles 's 'lock.sh' script.

tmpbg="$(mktemp --suffix=.png)"

scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
i3lock -i "$tmpbg"

rm "$tmpbg"
