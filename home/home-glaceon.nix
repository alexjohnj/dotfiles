{ pkgs, ... }:
{
  home.packages = with pkgs; [
    watchman
  ];

  programs.mise.enable = true;

  imports = [
    ./zed
  ];
}
