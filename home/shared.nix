{ config, pkgs, ... }: {
  home.username = "alex";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: [ dicts.en ]))
    bat
    fd
    file
    nil # nix lsp server
    nixfmt # "nix fmt" is built in but Emacs is looking for "nixfmt" which isn't.
    ripgrep
    tree
  ];

  imports =
    [ ./direnv ./emacs ./fish ./files ./git ./htop ./kitty ./tmux ./vim ];
}
