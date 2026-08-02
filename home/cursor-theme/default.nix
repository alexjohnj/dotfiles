{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.alexj.cursorTheme;
in
{
  options.alexj.cursorTheme = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "Yaru-purple-dark";
      description = "Cursor theme name (also used as the GTK widget/icon theme).";
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.yaru-theme;
      description = "Package providing alexj.cursorTheme.name.";
    };
    size = lib.mkOption {
      type = lib.types.int;
      default = 24;
      description = "Cursor size in pixels.";
    };
  };

  config = {
    # Exports XCURSOR_THEME/SIZE as session variables.
    home.pointerCursor = {
      enable = true;
      name = cfg.name;
      package = cfg.package;
      size = cfg.size;
      gtk.enable = true;
      x11.enable = true;
    };

    # Exports cursor settings to xwayland satellite clients
    services.xsettingsd = {
      enable = true;
      settings = {
        "Net/ThemeName" = cfg.name;
        "Gtk/CursorThemeName" = cfg.name;
        "Gtk/CursorThemeSize" = cfg.size;
      };
    };

    # Both this and xwayland-satellite.service are WantedBy
    # graphical-session.target with no ordering between them, so xsettingsd can
    # start and try to connect before Xwayland exists.
    systemd.user.services.xsettingsd.Unit.After = [ "xwayland-satellite.service" ];
  };
}
