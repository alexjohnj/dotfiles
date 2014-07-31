#!/usr/bin/env bash
# 
# This script installs the packages specified in EssentialCasks, EssentialCLIs and Fonts via homebrew
# It doesn't install the contents of Extras

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

if [ "$(uname -s)" == "Darwin" ] && test ! $(which brew)
then
	info "Installing Homebrew"
	ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
	success "Installing Homebrew"
else
	success "Homebrew already installed"
fi

brew bundle "$DOTFILES_ROOT/homebrew/EssentialCLIs"
success "Installed CLI Tools"

brew bundle "$DOTFILES_ROOT/homebrew/EssentialCasks"
success "Installed GUI Tools"

brew bundle "$DOTFILES_ROOT/homebrew/Fonts"
success "Installing Fonts"
