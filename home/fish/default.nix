{ pkgs, config, ... }: {
  programs.fish.enable = true;

  xdg.configFile."fish" = {
    recursive = true;
    source = ./config;
  };
}
