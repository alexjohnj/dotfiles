{ pkgs, ... }:
{
  home.packages = with pkgs; [
    inter
    noto-fonts
    overpass
    wl-clipboard-rs
    (callPackage ../packages/super-slicer/default.nix { })
    (callPackage ../packages/orca-slicer/default.nix { })
  ];

  programs.firefox.enable = true;

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
    ./swww
    ./waybar
    ./wlsunset
  ];
}
