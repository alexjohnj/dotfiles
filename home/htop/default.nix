{ config, pkgs, ... }:
{
  programs.htop = {
    enable = true;
    settings = {
      color_scheme = 6; # Broken Gray
    };
  };
}
