{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    git-lfs
    git-absorb
  ];

  programs.git.enable = true;
  xdg.configFile = {
    "git/config".source = ./gitconfig;
    "git/ignore".source = ./gitignore;
  };
}
