#!/usr/bin/env bash
#
# Based on @holman's bootstrap file: https://github.com/holman/dotfiles
# WARNING. Unlike @holman's script, this script will overwrite anything without asking the user.
# This installs Homebrew, fish shell and dotfiles in the repo
# After running this script, you'll need to run chsh to make fish your default shell

cd "$(dirname "$0")"
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
	info "Installing Homebrew"
	if [ $(uname -s) = "Darwin" ];then
		if ! type "brew" > /dev/null 2>&1;then
			ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
			success "Installing Homebrew"
		else
			success "Installing Homebrew (already installed)"
		fi
	else
		success "Not Installing Homebrew (incompatible platform)"
	fi
}

install_fish() {
	# Install fish if it isn't already
	info "Installing fish"
	if ! type "fish" > /dev/null 2>&1;then
		if [ $(uname -s) = "Darwin" ] && type "brew" > /dev/null 2>&1;then
			brew install fish
			success "Installing fish"
		elif [ $(uname -s) = "Linux" ] && type "pacman" > /dev/null 2>&1;then
			sudo pacman -S fish
			success "Installing fish"
		elif [ $(uname -s) = "Linux" ] && type "apt-get" > /dev/null 2>&1;then
			sudo apt-get install fish
			success "Installing fish"
		else
			fail "Installing fish (install manually)"
		fi
	else
		success "Installing fish (already installed)"
	fi

	# Install oh-my-fish if it isn't installed
	# This bit is essentially oh-my-fish's default install script converted to bash with the giant fish at the end removed.
	info "Installing oh-my-fish"
	if ! [ -d "$HOME/.oh-my-fish" ];then
		git clone https://github.com/bpinto/oh-my-fish.git $HOME/.oh-my-fish
		if [ -f $HOME/.config/fish/config.fish ];then
			mv $HOME/.config/fish/config.{fish,orig}
		fi
		cp $HOME/.oh-my-fish/templates/config.fish $HOME/.config/fish/config.fish
		success "Installing oh-my-fish"
	else
		success "Installing oh-my-fish (already installed)"
	fi

	# Install fish dotfiles
	fish_src="$DOTFILES_ROOT/fish/config.fish"
	fish_dst="$HOME/.config/fish/config.fish"
	fish_func_src="$DOTFILES_ROOT/fish/functions"
	fish_func_dst="$HOME/.config/fish/functions"

	info "Linking fish dotfiles"
	link_file "$fish_src" "$fish_dst"
	link_file "$fish_func_src" "$fish_func_dst"
	success "Linking fish dotfiles"
}

install_dotfiles() {
	for src in $(find "$DOTFILES_ROOT" -maxdepth 2 -name "*.symlink")
	do
	    dst="$HOME/.$(basename "${src%.*}")"
	    info "Linking $src to $dst"
	    link_file "$src" "$dst"
	    success "Linking $src to $dst"
	done
}

# Check git is installed
info "Checking git installation"
if ! type "git" > /dev/null 2>&1;then
	fail "Checking git installation"
	printf "Make sure git is installed and in your PATH\n"
fi
success "Checking git installation"

install_homebrew
install_fish
install_dotfiles
