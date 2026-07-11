{ pkgs, ... }:
{
  home.packages = with pkgs; [
    watchman
    macism
  ];
}
