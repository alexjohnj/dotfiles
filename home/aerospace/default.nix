{ pkgs, config, ... }:
let
  configHome = "${config.home.homeDirectory}/dotfiles/home/aerospace";
in
{
  home.packages = with pkgs; [
    aerospace
    jankyborders
  ];
  xdg.configFile."aerospace/aerospace.toml".source =
    config.lib.file.mkOutOfStoreSymlink "${configHome}/aerospace.toml";
}
