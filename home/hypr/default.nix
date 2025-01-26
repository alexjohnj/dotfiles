{
  config,
  pkgs,
  lib,
  ...
}:
{
  xdg.configFile."hypr/hyprland.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/hypr/hyprland.conf";

  xdg.configFile."hypr/hyprlock.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/hypr/hyprlock.conf";

  home.packages = with pkgs; [
    swww
    waypaper
  ];

  imports = [
    ./hyprsunset.nix
    ../rofi
  ];
}
