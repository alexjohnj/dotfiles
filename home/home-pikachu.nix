{ pkgs, ... }:
{
  home.packages = with pkgs; [
    inter
    overpass
    (callPackage ../packages/super-slicer/default.nix { })
  ];

  programs.firefox.enable = true;
  programs.zathura.enable = true;

  imports = [
    ./dunst
    ./gtk
    ./hypr
    ./waybar
  ];
}
