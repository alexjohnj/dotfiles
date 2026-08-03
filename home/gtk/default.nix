{ pkgs, ... }:
let
  noctaliaConfig = builtins.fromTOML (builtins.readFile ../noctalia/config.toml);
in
{
  gtk = {
    enable = true;
    font = {
      name = noctaliaConfig.shell.font_family;
      size = 12;
      package = pkgs.overpass;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    gtk4.theme = null;
  };
}
