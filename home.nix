{ config, pkgs, ... }: {
  home.username = "alex";
  home.homeDirectory = "/home/alex";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
