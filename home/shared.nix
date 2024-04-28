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

  programs = {
    emacs.enable = true;

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    htop = {
      enable = true;
      settings = {
        color_scheme = 6; # Broken Gray
      };
    };
  };

  home.file = {
    ".gemrc".source = ./config/gemrc;
    ".ideavimrc".source = ./config/ideavimrc;
    ".stglobalignore".source = ./config/stglobalignore;
  };

  imports = [ ./git ./vim ./kitty ];
}
