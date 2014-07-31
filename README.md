# dotfiles

This is my attempt at a dotfiles repository. It's mainly to keep my computers in sync but other people may find it useful. My shell of choice is [fish][fish-shell] so there's no zsh or bash config files in here. Saying that, most of the install scripts are bash scripts for out of the box compatibility.

## Installation

_N.B. You may (probably) need to install git before doing anything. If you're on OS X, install the Xcode command line tools._

To get started, **make a backup of your current configuration files**. Then, run `bootstrap.sh` from the root directory. `bootstrap.sh` **will not** ask you if you want to overwrite existing configuration files. It will just do it. `bootstrap.sh` will do the following:

- Install [homebrew][homebrew-link] if you're on OS X and haven't got it installed.
- Install [fish][fish-shell] if you're on OS X, have homebrew and haven't got it installed.
- Install [oh-my-fish][oh-my-fish-link] if you haven't got it installed.
- Symlink the `fish/config.fish` file to `~/.config/fish/config.fish`
- Symlink the `fish/functions/` folder to `~/.config/fish/functions/`
- Symlink `*.symlink` files in the repo to `~/.*`

`bootstrap.sh` _should_ work on Linux as well as OS X, it just won't install `fish` or `homebrew`.

You're probably going to want to modify `fish/functions/aliases.fish` since these are specific to my projects.

## Other Things

There's a few other scripts in the repository. `homebrew/install.sh` will install a variety of packages, apps and fonts using homebrew and homebrew cask. Read the `Brewfile`/`Caskfile`/`Fontfile` to see what gets installed. If you only want to install one of those files, you can run `brew bundle Somethingfile` to install a specific file.

`sublime-text/install.sh` will symlink your Sublime Text Preferences and Package Control Preferences files to the repository. Adding a package to `Package Control.sublime-settings` will cause it to be installed when you next start Sublime Text. Note that this script only works on OS X.

Finally there's `osx/defaults.sh`. This script changes a few of OS X's settings to my liking. It's pretty bare-bones at the moment but should grow over time.

[fish-shell]: http://fishshell.com

[homebrew-link]: http://brew.sh

[oh-my-fish-link]: https://github.com/bpinto/oh-my-fish

## Credits

The way I organised this repository and the bootstrap script is based off of [Zach Holman's dotfiles repository][zach-dotfiles].

[zach-dotfiles]: https://github.com/holman/dotfiles 

