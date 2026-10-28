{ pkgs, ... }:
{
  home.packages = with pkgs; [
    watchman
  ];

  programs.mise.enable = true;

  targets.darwin = {
    copyApps.enable = true;
    linkApps.enable = false;
  };

  imports = [
    ./zed
  ];
}
