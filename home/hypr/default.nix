{
  config,
  pkgs,
  lib,
  ...
}:
{
  xdg.configFile."hypr/hyprland.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/hypr/hyprland.conf";

  xdg.configFile."hypr/hyprlock.conf".source = ./hyprlock.conf;

  home.packages = with pkgs; [
    swww
    waypaper
  ];

  services.hypridle.enable = true;
  xdg.configFile."hypr/hypridle.conf".source = ./hypridle.conf;

  imports = [
    ./hyprsunset.nix
    ../rofi
  ];
}
