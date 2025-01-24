{ config, ... }:
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      mainBar = {
        layer = "top";
        modules-left = [
          "hyprland/workspaces"
          "hyprland/submap"
        ];
        modules-center = [ ];
        modules-right = [
          "clock"
        ];
        clock = {
          format = "{:%a %d %b  %H:%M}";
        };
      };
    };
  };

  xdg.configFile."waybar/style.css".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/waybar/style.css";
}
