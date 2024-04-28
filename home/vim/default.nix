{ pkgs, ... }: {
  programs.vim = {
    enable = true;
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
