#!/bin/sh
# Based on @holman's bootstrap file:
# https://github.com/holman/dotfiles WARNING. Unlike @holman's script,
# this script will overwrite previously installed dotfiles. This
# installs Homebrew, fish shell and dotfiles in the repo After running
# this script, you'll need to run chsh to make fish your default shell.

cd "$(dirname "$0")" || exit
DOTFILES_ROOT=$(pwd)

success() {
  printf "\r\033[2K  [ \033[00;32mOK\033[0m ] %s\n" "$1"
}

info() {
  printf "\r\033[2k  [ \033[00;34m..\033[0m ] %s" "$1"
}

fail() {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] %s\n" "$1"
  echo ''
  exit
}

fail_c() {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] %s\n" "$1"
}

link_file() {
    local src=$1 dst=$2

    if [ ! -d "$(dirname "$dst")" ]
    then
        mkdir -p "$(dirname "$dst")"
    fi

    if [ -f "$dst" ] || [ -d "$dst" ] || [ -L "$dst" ]
    then
        rm -rf "$dst" || exit
    fi
    ln -s "$src" "$dst" || exit
}

install_dotfiles() {
    for src in $(find "$DOTFILES_ROOT" -maxdepth 2 -name "*.symlink")
    do
        dst="$HOME/.$(basename "${src%.symlink}")"
        link_file "$src" "$dst"
        if [ $? -eq 1 ]
        then
           fail_c "Linking $src to $dst"
        else
            success "Linking $src to $dst"
        fi
    done
}

install_homebrew() {
    if ! [ "$(uname -s)" = "Darwin" ] || type "brew" > /dev/null 2>&1
    then
        return
    fi

    while true
    do
        printf "Install homebrew? [y/n] > "
        read yn
        case $yn in
             [Yy]* ) break;;
             [Nn]* ) return;;
        esac
    done

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    if [ $? -ne 0 ]
    then
       info "Failed to install homebrew"
       fail_c "Failed to install homebrew"
    else
        info "Installed homebrew"
        success "Installed homebrew"
    fi
}

install_fish() {
    if type "fish" > /dev/null 2>&1
    then
        return
    fi

    if ! [ $(uname -s) = "Darwin" ] || ! type "brew" > /dev/null 2>&1
    then
        return
    fi

    while true
    do
        printf "Install fish? [y/n] > "
        read yn
        case $yn in
             [Yy]* ) break;;
             [Nn]* ) return;;
        esac
    done

    brew install fish
    if [ $? -ne 0 ]
    then
        info "Failed to install fish"
        fail_c "Failed to install fish"
    else
        info "Installed fish"
        success "Installed fish"
    fi
        
}

install_fish_dotfiles() {
    while true
    do
        printf "Install fish dotfiles? [y/n] > "
        read yn
        case $yn in
            [Yy]* ) break;;
            [Nn]* ) return;;
        esac
    done

	fish_src="$DOTFILES_ROOT/fish/config.fish"
	fish_dst="$HOME/.config/fish/config.fish"
	fish_func_src="$DOTFILES_ROOT/fish/functions"
	fish_func_dst="$HOME/.config/fish/functions"

	info "Linking fish dotfiles"
	link_file "$fish_src" "$fish_dst"
	link_file "$fish_func_src" "$fish_func_dst"
	success "Linking fish dotfiles"
}

printf "\033[0;31mWARNING.\033[0m This script will overwrite any previously installed dotfiles!\nContinue? [y/n] >> "
while true
do
    read yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) printf "Enter [y/n] >> "
    esac
done


install_fish
install_fish_dotfiles
install_dotfiles
