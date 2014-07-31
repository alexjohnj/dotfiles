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

install_homebrew() {
	if [ "$(uname -s)" == "Darwin" ] && test ! $(which brew)
	then
		info "Installing Homebrew"
		ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
		success "Installing Homebrew"
	else
		success "Homebrew already installed"
	fi
}

install_fish() {
	# Install oh-my-fish if it isn't installed
	if [ ! -d "$HOME/.oh-my-fish" ]
	then
		info "Installing oh-my-fish"
		curl -L https://github.com/bpinto/oh-my-fish/raw/master/tools/install.fish | fish
		success "Installing oh-my-fish"
	else
		success "oh-my-fish already installed"
	fi

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
}

install_homebrew
install_fish

