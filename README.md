# dotfiles

This is my attempt at a dotfiles repository. It's mainly to keep my
computers in sync but other people may find it useful. It contains
configuration files for fish, Emacs (main text editor), vim (back-up
editor) and tmux. The configuration files target OS X but try and
maintain compatibility with other UNIX operating systems.

## Installation

To get a new system up and running, there's a script called
`bootstrap.sh` in the root of the project. The bootstrap script will
symlink the configuration files to your home folder. **It'll overwrite
any existing configuration files so make sure you have a backup.** The
bootstrap script also attempts to install the following tools:

- [Homebrew][homebrew-link] if you're on OS X.
- [Fish][fish-shell] if you're on OS X.

[fish-shell]: http://fishshell.com
[homebrew-link]: http://brew.sh

The script is written in `sh` and so should work on pretty much any
system out of the box. If you want the script to handle installing
fish and Homebrew, you'll also need to have `git` installed.

## Things to Modify

Obviously some of these dotfiles are specific to my set up and you'll
need to change them. The big one would be `git/gitconfig.symlink`
which has my email address and name in it. You'll want to change that
to your details. The other thing would be some of the aliases in
`fish/functions/aliases.fish`. Some of these are shortcuts to projects
on my computer, so you'll want to get rid of them.

## Other Things

There's some additional scripts to be found in the repository. In the
`osx` directory, there's a script called `osx.sh` which applies a
bunch of settings to OS X. There's also `osx-revert.sh` which reverts
them.

In the `homebrew` directory there's a script called
`install.sh`. Using homebrew, this installs the packages listed in
`Brewfile` using the supplied arguments.

## Credits

The way I organised this repository and the bootstrap script is based
off of [Zach Holman's dotfiles repository][zach-dotfiles].

[zach-dotfiles]: https://github.com/holman/dotfiles
