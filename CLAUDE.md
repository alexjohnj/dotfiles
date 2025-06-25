# CLAUDE.md

Instructions for Claude and other agents working in this repository.

## Purpose

This is a personal dotfiles repository managed with Nix using flakes. It
configures NixOS systems ("hosts") and installs user config files using Home
Manager.

## Structure

- `flake.nix` Defines dependencies and outputs for each host.
- `hosts/` Configuration for each NixOS system.
- `home/` Home Manager config for user level packages and dotfiles shared
  across multiple machines. Config for each application is stored in a
  subdirectory.
  - `home/default.nix` Base home configuration
  - `home/home-pikachu.nix` Home configuration specific for Pikachu.
- `modules/` Reusable NixOS modules.
- `packages/` Custom Nix derivations for applications not in nixpkgs.
- `nix-secrets/`: Private repository containing secrets managed using `agenix`
  (https://github.com/ryantm/agenix).
  
## Important Programs

The following programs are used across all hosts.

- `home/emacs/`: Primary text editor. Config is installed on hosts via a symlink instead of through nix.
- `home/vim/`: Secondary editor for quick edits.
- `home/fish/`: Shell of choice.
- `home/ghostty/` Terminal emulator of choice.
- `home/zed/`: Used as an IDE.

## Hosts

### Glaceon

An M1 MacBook Pro managed using Home Manager.

### Pikachu

A NixOS desktop using an NVIDIA graphics card and an AMD Ryzen CPU. Key programs
used on Pikachu include:

- `home/niri` as its window manager (https://yalter.github.io/niri/).
- `home/dunst` to display notifications.
- `home/rofi` as an application launcher.
- `home/waybar` as the menu bar.

## Development Workflow

```sh
# 1. Make changes.

# 2. Check nix files
nix flake check

# 3. Format nix files
nix fmt **/*.nix
```

