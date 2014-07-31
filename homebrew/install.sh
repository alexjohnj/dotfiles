#!/usr/bin/env bash

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

brew bundle "$DOTFILES_ROOT/homebrew/Brewfile"
success "Installed CLI Tools"

brew bundle "$DOTFILES_ROOT/homebrew/Caskfile"
success "Installed GUI Tools"

info "Installed Fonts"
brew bundle "$DOTFILES_ROOT/homebrew/Fontfile"
success "Installing Fonts"
