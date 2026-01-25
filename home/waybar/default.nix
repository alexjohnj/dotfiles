{ config, ... }:
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      mainBar = {
        layer = "top";
        "custom/launcher" = {
          format = "󱄅";
          tooltip = false;
          on-click = "pkill rofi || rofi -show drun -theme-str 'window { location: north west; anchor: north; y-offset: 8px; x-offset: 16px; }'";
        };
        modules-left = [
          "custom/launcher"
          "niri/workspaces"
          "niri/window"
        ];
        modules-right = [
          "tray"
          "clock"
        ];
        tray = {
          icon-size = 16;
          spacing = 8;
        };
        clock = {
          format = "{:%a %d %b  %H:%M}";
        };
      };
    };
  };

  xdg.configFile."waybar/style.css".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/waybar/style.css";
}
