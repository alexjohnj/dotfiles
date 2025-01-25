{ config, ... }:
{
  services.dunst = {
    enable = true;
  };

  xdg.configFile."dunst/dunstrc".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/dunst/dunstrc";
}
