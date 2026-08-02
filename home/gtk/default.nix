{ config, ... }:
let
  cfg = config.alexj.cursorTheme;
in
{
  gtk = {
    enable = true;
    font.name = "Overpass 12";
    theme = {
      name = cfg.name;
      package = cfg.package;
    };
    iconTheme = {
      name = cfg.name;
      package = cfg.package;
    };
    gtk4.theme = null;
  };
}
