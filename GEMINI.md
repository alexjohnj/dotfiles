# GEMINI.md

## High-Level Summary

This is a personal dotfiles repository managed with Nix, NixOS, and Home
Manager, using the Flakes feature for reproducibility. It configures two primary
systems: a NixOS desktop named `pikachu` and a macOS machine named `glaceon`.

The configuration is highly modular, covering the shell (Fish), editors (Emacs,
Zed, Vim), desktop environments (Hyprland on Linux, Aerospace on macOS), and
various command-line tools. Secrets are managed using `agenix` with a separate
private Git repository.

The core philosophy is to share as much configuration as possible via a common
Home Manager profile in `home/`, with machine-specific overrides where
necessary.

## Key Technologies

- **Configuration Management**: Nix, NixOS, Home Manager, Nix Flakes
- **Secret Management**: `agenix`
- **Editors**: Emacs (configured with `straight.el`), Zed, Vim
- **Shell**: Fish Shell
- **Desktop Environment (Linux)**: Hyprland (Wayland Compositor), Waybar, Rofi,
  Dunst
- **Window Manager (macOS)**: Aerospace
- **Key Applications**: Git, Kitty, Alacritty, Tmux, Firefox

## Hosts

- `pikachu` A NixOS desktop with an NVIDIA graphics card.
- `glaceon` An M1 MacBook Pro running home-manager.

## Repository Structure

The repository is organized around the Nix flake (`flake.nix`) which serves as
the main entry point.

-   `flake.nix`: The heart of the repository. It defines dependencies (nixpkgs,
    home-manager, agenix), and builds two main outputs:
    -   `nixosConfigurations.pikachu`: The full NixOS system configuration for
        the desktop.
    -   `homeConfigurations."alex@glaceon"`: The Home Manager configuration for
        the user on the macOS machine.

-   `hosts/`: Contains machine-specific NixOS configurations.
    -   `pikachu/configuration.nix`: Defines the system-level configuration for
        the NixOS machine, including hardware, networking, system services
        (greetd, pipewire, syncthing), backups (restic), and imports the shared
        home-manager profile.

-   `home/`: Contains the shared Home Manager configuration for user-level
    packages and dotfiles. This is where most of the application configurations
    live. It's structured by application:
    -   `default.nix`: The main entry point for the shared home profile. It
        lists common packages and imports other modules.
    -   `home-pikachu.nix`: A profile specific to the `pikachu` machine, layered
        on top of the shared profile.
    -   `emacs/`: A comprehensive Emacs configuration.
    -   `fish/`: Configuration for the Fish shell.
    -   `hypr/`: Configuration for the Hyprland window manager and related tools
        (Waybar, Rofi).
    -   `aerospace/`: Configuration for the Aerospace tiling window manager on
        macOS.
    -   Other directories for tools like `git`, `zed`, `tmux`, etc.

-   `modules/`: Reusable NixOS modules.
    -   `system/nvidia.nix`: A module to configure NVIDIA drivers on NixOS.

-   `packages/`: Custom Nix derivations for applications not readily available
    in nixpkgs, such as `orca-slicer` and `super-slicer`.

-   `README.md`: A high-level overview of the repository for human readers.

## Key Configurations Deep Dive

### Nix and Home Manager

The setup is a prime example of a flake-based dotfiles repository.

-   **Shared vs. Specific Config**: The `home/default.nix` file provides the
    base configuration for all systems. The `hosts/pikachu/configuration.nix`
    file then imports this base and layers its specific configurations on top,
    including the `home/home-pikachu.nix` additions.
-   **Secrets**: The `flake.nix` file references a private Git repository for
    secrets, which are managed by `agenix`. The `age.secrets` block in
    `hosts/pikachu/configuration.nix` shows how these encrypted files are
    deployed to the system.
-   **Symlinking**: The configuration uses `config.lib.file.mkOutOfStoreSymlink`
    for some directories (like `emacs.d`). This is a key detail: it means these
    configurations can be edited in place without requiring a full
    `nixos-rebuild`, which is useful for rapid iteration (e.g., on Emacs
    config).

### Emacs Configuration (`home/emacs/`)

The Emacs setup is extensive and a central piece of the dotfiles.

-   **Management**: It is not packaged by Nix, but rather symlinked into the
    home directory via Home Manager. This allows for in-place editing and
    package management from within Emacs itself.
-   **Package Manager**: It uses `straight.el` for package management, with
    versions pinned in `emacs.d/straight/versions/default.el`.
-   **Structure**:
    -   `init.el`: The main entry point.
    -   `early-init.el`: For performance-critical settings loaded before package
        initialization.
    -   `package-config/`: Configuration is modularized here, with one file per
        major package or feature (e.g., `init-evil.el`, `init-projectile.el`,
        `init-languages.el`).
-   **Key Features**:
    -   Evil mode for Vim emulation.
    -   Modern completion framework using Vertico, Corfu, Orderless, and
        Marginalia.
    -   LSP support via `eglot`.
    -   Leader-key based bindings (`SPC`) managed by `general.el`.
    -   Language support is configured in `init-languages.el`, utilizing
        tree-sitter for many modes.

### Shell Configuration (`home/fish/`)

The Fish shell is the default shell.

-   `config.fish`: The main entry point, responsible for initializing the
    environment (`mise`, `direnv`, `fzf`).
-   `exports.fish`: Defines environment variables (`PATH`, `EDITOR`, etc.).
-   `abbreviations.fish`: Contains aliases and abbreviations.
-   `functions/`: Contains custom shell functions, with each function in its own
    file. The prompt is defined in `fish_prompt.fish`.

### Desktop Environments (Hyprland & Aerospace)

The repository configures two different window management systems for two
different OSs.

-   **Hyprland (Linux)**: The `home/hypr/` directory contains `hyprland.conf`
    which defines keybindings, window rules, and visual settings. It is tightly
    integrated with `waybar` (status bar), `rofi` (launcher), `dunst`
    (notifications), and `hypridle`/`hyprlock` (idle/locking).
-   **Aerospace (macOS)**: The `home/aerospace/` directory contains
    `aerospace.toml`, which configures this tiling window manager for macOS with
    its own set of keybindings and rules.

## How to Make Changes

Here are guidelines for making common changes to this repository.

-   **To Add a Package (User-level, for all systems):** Add the package to the
    `home.packages` list in `home/default.nix`.

-   **To Add a Package (Pikachu System-level):** Add the package to
    `environment.systemPackages` in `hosts/pikachu/configuration.nix`.

-   **To Modify an Application's Configuration:**
    1.  Locate the corresponding application directory under `home/` (e.g.,
        `home/tmux/` for tmux).
    2.  Edit the configuration file (e.g., `tmux.conf`).
    3.  Run `nixos-rebuild switch` (on Pikachu) or `home-manager switch` (on
        Glaceon) to apply the changes.

-   **To Modify the Emacs Configuration:**
    1.  Locate the relevant file in `home/emacs/emacs.d/` (e.g.,
        `package-config/init-org.el` for Org mode).
    2.  Edit the file.
    3.  Since this directory is symlinked, you only need to restart Emacs to see
        the changes. No Nix rebuild is necessary.
    4.  To add a new package, add a `(use-package ...)` declaration in `init.el`
        or a relevant module file. Then run `M-x straight-use-package` or
        restart Emacs.

-   **To Update Dependencies:**
    1.  Run `nix flake update` to fetch the latest versions of inputs specified
        in `flake.nix` and update `flake.lock`.
    2.  Rebuild the system to apply the updates.

-   **To Add a New Secret:**
    1.  Add the encrypted file to the private `nix-secrets` repository.
    2.  Reference it in `hosts/pikachu/configuration.nix` within the
        `age.secrets` block to have `agenix` decrypt and deploy it.

## Rules

- Whenever unsure about something that's related to the project, ask me for clarification before making changes.
