#!/usr/bin/env bash
#

cd "$(dirname "$0")/.."
DOTFILES_ROOT=$(pwd)

set -e

success () {
  printf "\r\033[2K  [ \033[00;32mOK\033[0m ] $1\n"
}

info () {
  printf "  [ \033[00;34m..\033[0m ] $1"
}

fail () {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] $1\n"
  echo ''
  exit
}

link_file() {
	local src=$1 dst=$2

	# Make sure parent directories exist

	if [ ! -d "$(dirname "$dst")" ]
	then
		mkdir -p "$(dirname "$dst")"
	fi

	if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]
	then
		rm -rf "$dst"
	fi

	ln -s "$src" "$dst"
}

if [ ! $(uname -s) == "Darwin" ]
then
	fail "Sorry, this script only works on OS X"
fi

pref_src="$DOTFILES_ROOT/sublime-text/Preferences.sublime-settings"
pref_dst="$HOME/Library/Application Support/Sublime Text 3/Packages/User/Preferences.sublime-settings"

link_file "$pref_src" "$pref_dst"

pkg_src="$DOTFILES_ROOT/sublime-text/Package Control.sublime-settings"
pkg_dst="$HOME/Library/Application Support/Sublime Text 3/Packages/User/Package Control.sublime-settings"

link_file "$pkg_src" "$pkg_dst"
