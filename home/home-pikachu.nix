{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    [
      discord
      inter
      overpass
      (callPackage ../packages/orca-slicer/default.nix { })
      (callPackage ../packages/super-slicer/default.nix { })
    ]
    ++ (with pkgs.gnomeExtensions; [
      appindicator
      blur-my-shell
      dash-to-dock
    ]);

  programs.firefox.enable = true;
  programs.zathura.enable = true;

  imports = [
    ./dunst
    ./gtk
    ./hypr
    ./waybar
  ];
}
