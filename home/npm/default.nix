{ config, ... }:
{
  home.file.".npmrc".text = ''
    ignore-scripts=true
    prefix=${config.home.homeDirectory}/.npm-global
  '';
}
