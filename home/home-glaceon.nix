{ pkgs, ... }:
{
  home.packages = with pkgs; [
    watchman
    macism
  ];

  programs.mise.enable = true;

  imports = [
    ./zed
  ];
}
