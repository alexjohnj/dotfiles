{ config, pkgs, ... }: {
  home.username = "alex";
  home.homeDirectory = if pkgs.stdenv.hostPlatform.isLinux then
    "/home/alex"
  else if pkgs.stdenv.hostPlatform.isDarwin then
    "/Users/alex"
  else
    builtins.abort "Unsupported platform";

  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    asdf-vm
    (aspellWithDicts (dicts: [ dicts.en ]))
    bat
    du-dust
    fd
    file
    iperf
    multimarkdown
    nil # nix lsp server
    nixfmt # "nix fmt" is built in but Emacs is looking for "nixfmt" which isn't.
    ripgrep
    tree
    yt-dlp
  ];

  imports =
    [ ./direnv ./emacs ./fish ./files ./git ./htop ./kitty ./tmux ./vim ];
}
