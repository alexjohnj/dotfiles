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

# Link ./config/*.slink to $HOME/.config/
install_config_directory() {
    for file in "$DOTFILES_ROOT"/config/*.symlink
    do
        destination="$HOME/.config/$(basename $file .symlink)"
        link_file "$file" "$destination"
        if [ $? -eq 1 ]
        then
            fail_c "Linking $file to $destination"
        else
            success "Linking $file to $destination"
        fi
    done
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

install_dotfiles
install_config_directory
