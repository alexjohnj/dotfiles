{ pkgs, config, ... }:
{
  programs.fish.enable = true;
  programs.fzf = {
    enable = true;
    defaultOptions = [ "--reverse" ];
  };

  xdg.configFile."fish" = {
    recursive = true;
    source = ./config;
  };
}
