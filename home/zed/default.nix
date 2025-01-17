{
  pkgs,
  ...
}:
{
  programs.zed-editor.enable = pkgs.stdenv.hostPlatform.isLinux;
  xdg.configFile = {
    "zed/keymap.json".source = ./keymap.json;
    "zed/settings.json".source = ./settings.json;
  };
}
