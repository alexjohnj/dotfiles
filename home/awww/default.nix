{ pkgs, ... }:
{
  services.awww.enable = true;

  home.packages = with pkgs; [
    waypaper
  ];
}
