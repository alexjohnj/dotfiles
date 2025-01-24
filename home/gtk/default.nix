{ pkgs, ... }:
{
  gtk = {
    enable = true;
    font.name = "Overpass 12";
    theme = {
      name = "Yaru-purple-dark";
      package = pkgs.yaru-theme;
    };
    iconTheme = {
      name = "Yaru-purple-dark";
      package = pkgs.yaru-theme;
    };
    cursorTheme = {
      name = "Yaru-purple-dark";
      package = pkgs.yaru-theme;
      size = 24;
    };
  };
}
