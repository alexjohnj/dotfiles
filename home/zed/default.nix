{ config, ... }:
let
  zedHome = "${config.home.homeDirectory}/dotfiles/home/zed";
in
{
  xdg.configFile = {
    "zed/keymap.json".source = config.lib.file.mkOutOfStoreSymlink "${zedHome}/keymap.json";
    "zed/settings.json".source = config.lib.file.mkOutOfStoreSymlink "${zedHome}/settings.json";
  };
}
