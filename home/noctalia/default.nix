{
  config,
  pkgs,
  noctalia,
  ...
}:
{
  programs.noctalia = {
    enable = true;
    package = noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default;
    systemd.enable = true;
  };

  xdg.configFile."noctalia/config.toml".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/noctalia/config.toml";
}
