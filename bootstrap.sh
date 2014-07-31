#!/usr/bin/env bash
#
# Based on @holman's bootstrap file: https://github.com/holman/dotfiles
# WARNING. Unlike @holman's script, this script will overwrite anything without asking the user.

cd "$(dirname "$0")"
DOTFILES_ROOT=$(pwd)

set -e

success () {
  printf "\r\033[2K  [ \033[00;32mOK\033[0m ] $1\n"
}

info () {
  printf "  [ \033[00;34m..\033[0m ] $1"
}

link_file() {
	local src=$1 dst=$2

	# Make sure parent directories exist

	if [ ! -d $(dirname "$dst") ]
	then
		mkdir -p $(dirname "$dst")
	fi

	if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]
	then
		rm -rf "$dst"
	fi

	ln -s "$src" "$dst"
}

# Install fish dotfiles
fish_src="$DOTFILES_ROOT/fish/config.fish"
fish_dst="$HOME/.config/fish/config.fish"

info "Installing fish config"
link_file "$fish_src" "$fish_dst"
success "Linked fish config"

# Install fish functions folder
fish_func_src="$DOTFILES_ROOT/fish/functions"
fish_func_dst="$HOME/.config/fish/functions"

info "Installing fish functions"
link_file "$fish_func_src" "$fish_func_dst"
success "Installing fish functions"