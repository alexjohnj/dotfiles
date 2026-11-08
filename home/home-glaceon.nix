{ pkgs, ... }:
{
  home.packages = with pkgs; [
    watchman
  ];

  programs.mise.enable = true;

  programs.claude-code.settings = {
    model = "opus";
    advisorModel = "fable";
  };

  targets.darwin = {
    copyApps.enable = true;
    linkApps.enable = false;
  };

  imports = [
    ./zed
  ];
}
