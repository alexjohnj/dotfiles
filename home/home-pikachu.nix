{ pkgs, ... }:
{
  home.packages = with pkgs; [
    inter
    overpass
    noto-fonts
    (callPackage ../packages/super-slicer/default.nix { })
  ];

  programs.firefox.enable = true;

  imports = [
    ./dunst
    ./gtk
    ./hypr
    ./waybar
  ];
}
