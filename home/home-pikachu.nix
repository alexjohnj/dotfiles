{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    docker-credential-helpers
    inter
    noto-fonts
    overpass
    wl-clipboard-rs
    (callPackage ../packages/super-slicer/default.nix { })
  ];

  home.file.".config/containers/auth.json".text = builtins.toJSON {
    credHelpers = {
      "registry.gitlab.com" = "secretservice";
    };
  };

  programs.firefox = {
    enable = true;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
  };

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-mozc-ut # Japanese
        fcitx5-gtk
      ];
    };
  };

  imports = [
    ./dunst
    ./gtk
    ./hypr
    ./niri
    ./awww
    ./waybar
    ./wlsunset
  ];
}
