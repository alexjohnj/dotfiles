{ config, pkgs, ... }:
{
  home.username = "alex";
  home.homeDirectory =
    if pkgs.stdenv.hostPlatform.isLinux then
      "/home/alex"
    else if pkgs.stdenv.hostPlatform.isDarwin then
      "/Users/alex"
    else
      builtins.abort "Unsupported platform";

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: [ dicts.en ]))
    bat
    btop
    du-dust
    fd
    file
    imagemagick
    iperf
    less
    multimarkdown
    nh
    nil
    nixd
    nixfmt-rfc-style # "nix fmt" is built in but Emacs is looking for "nixfmt" which isn't.
    nodejs_22
    pandoc
    ripgrep
    tmux
    tree
    yt-dlp
  ];

  programs.mise.enable = true;

  alexj.ghostty.enable = true;

  imports = [
    ./direnv
    ./emacs
    ./files
    ./fish
    ./ghostty
    ./git
    ./htop
    ./ssh
    ./tmux
    ./vim
    ./zed
  ];
}
