{ config, ... }:
{
  xdg.configFile."hypr/hyprland.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/hyprland/hyprland.conf";

  xdg.configFile."hypr/hyprlock.conf".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/hyprland/hyprlock.conf";

  services.hyprpaper = {
    enable = true;
    settings =
      let
        image = "~/Pictures/Wallpapers/Drawings/6K - Fluted Gradients - Orange.png";
      in
      {
        ipc = "off";
        preload = [ image ];
        wallpaper = [
          ",${image}"
        ];
      };
  };

  imports = [
    ../rofi
  ];
}
