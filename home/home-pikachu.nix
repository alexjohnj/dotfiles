{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    _1password-gui
    discord
    f3d
    overpass
    (callPackage ../packages/super-slicer/default.nix { })
  ];
  programs.firefox.enable = true;
}
