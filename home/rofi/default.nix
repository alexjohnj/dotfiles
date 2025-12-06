{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    (rofi.override {
      plugins = [
        rofi-calc
      ];
    })
    rofi-power-menu
  ];

  xdg.configFile."rofi/config.rasi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/rofi/config.rasi";

  xdg.configFile."rofi/catppuccin-mocha.rasi".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/rofi/catppuccin-mocha.rasi";
}
