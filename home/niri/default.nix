{
  config,
  ...
}:
{
  xdg.configFile."niri/config.kdl".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/niri/config.kdl";

  # Included by main config file
  xdg.configFile."niri/cursor.kdl".text =
    let
      cfg = config.alexj.cursorTheme;
    in
    ''
      cursor {
          xcursor-theme "${cfg.name}"
          xcursor-size ${toString cfg.size}
      }
    '';
}
