{ config, pkgs, ... }: {
  home.username = "alex";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: [ dicts.en ]))
    bat
    fd
    git
    htop
    kitty
    nil # nix lsp server
    nixfmt # "nix fmt" is built in but Emacs is looking for "nixfmt" which isn't.
    ripgrep
    tree
    vim
  ];

  programs = {
    emacs.enable = true;

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
