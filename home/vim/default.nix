{ pkgs, ... }:
{
  programs.vim = {
    enable = true;
    packageConfigurable = pkgs.vim;
    extraConfig = builtins.readFile ./vimrc;
    plugins = with pkgs.vimPlugins; [
      ctrlp-vim
      vim-commentary
      vim-fish
      vim-nix
      vim-surround
    ];
  };
}
