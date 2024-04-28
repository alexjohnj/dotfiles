{ pkgs, config, ...}: {
  programs.tmux = {
    enable = false;
    extraConfig = builtins.readFile ./tmux.conf;
  };
}
