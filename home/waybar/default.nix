{ ... }:
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
    style = ./style.css;
  };
}
