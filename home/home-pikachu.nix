{ config, pkgs, ... }:
{
  home.packages =
    with pkgs;
    [
      _1password-gui
      discord
      f3d
      overpass
      inter
      (callPackage ../packages/orca-slicer/default.nix { })
      (callPackage ../packages/prusa-slicer/default.nix { })
      (callPackage ../packages/super-slicer/default.nix { })
    ]
    ++ (with pkgs.gnomeExtensions; [
      appindicator
      blur-my-shell
      dash-to-dock
    ]);

  programs.firefox.enable = true;
}
