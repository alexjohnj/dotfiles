{
  pkgs,
  config,
  ...
}:
let
  zedHome = "${config.home.homeDirectory}/dotfiles/home/zed";
in
{
  programs.zed-editor.enable = pkgs.stdenv.hostPlatform.isLinux;
  xdg.configFile = {
    "zed/keymap.json".source = config.lib.file.mkOutOfStoreSymlink "${zedHome}/keymap.json";
    "zed/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${zedHome}/settings.json";
  };
}
