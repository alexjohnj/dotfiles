{ config, pkgs, ... }: {
  imports = [ ./git ./vim ];

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

    kitty = {
      enable = true;
      font.name = "Zed Mono Extended";
      font.size = 13.0;
      settings = { window_padding_width = "5 10"; };
      # Theme names ares specified in github.com/kovidgoyal/kitty-themes
      # Theme previews can be found in github.com/dexpota/kitty-themes
      theme = "Tomorrow Night Eighties";
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
}
