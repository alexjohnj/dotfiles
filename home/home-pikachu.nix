{ config, pkgs, ... }: {
  home.homeDirectory = "/home/alex";
  home.packages = with pkgs; [ _1password-gui discord f3d overpass ];

  programs.firefox.enable = true;
}
