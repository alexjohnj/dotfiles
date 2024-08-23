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
      dash-to-dock
      blur-my-shell
      vitals
    ]);

  programs.firefox.enable = true;
}
