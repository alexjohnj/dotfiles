# dotfiles

This is the configuration for my computing life. This repo includes the config
for the various command line tools that I use and the configuration for my
computers that run NixOS. It's using Home Manager to configure and install
applications on both a MacBook (`glaceon`) and a NixOS system (`pikachu`).

Most of what's in this repository will be of little interest or use to anyone
who isn't me. The bits you may find interesting are:

- The [`emacs.d`
  directory](https://github.com/alexjohnj/dotfiles/tree/master/home/emacs/emacs.d)
  containing my Emacs configuration.
- My Fish shell
  [configuration](https://github.com/alexjohnj/dotfiles/tree/master/home/fish/config).
- The
  [configuration](https://github.com/alexjohnj/dotfiles/blob/master/hosts/pikachu/configuration.nix)
  for Pikachu, my desktop computer that runs NixOS.

Do not expect to be able to clone this repository and use it directly. For a
starter, it requires access to a private Git repository containing encrypted
secrets. Instead, pick and choose the bits that seem useful and add them to your
own dotfiles.
