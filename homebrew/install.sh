#!/usr/bin/env bash
# 
# Installs the brews in Brewfile. NB: You need to tap Homebrew/brewdler 
# before running this script.
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

cd "$(dirname "$0")/.."
DOTFILES_ROOT=$(pwd)

info "Tapping Homebrew/brewdler"
if [ "$(uname -s)" != "Darwin" ];then
  fail "Tapping Homebrew/brewdler (wrong platform)"
fi

if ! type "brew" > /dev/null 2>&1;then
  fail "Tapping Homebrew/brewdler (Homebrew not installed)"
fi
brew tap Homebrew/brewdler

if [ $? -ne -];then
  fail "Tapping Homebrew/brewdler"
else
  success "Tapping Homebrew/brewdler"
fi

info "Installing Brewfile"
cd "$DOTFILES_ROOT/homebrew/"
brew brewdle

if [ $? -ne 0 ];then
  fail "Installing Brewfile"
else
  success "Installing Brewfile"  
fi
