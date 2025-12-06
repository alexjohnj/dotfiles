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
