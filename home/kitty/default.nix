{ config, pkgs, ... }: {
  programs.kitty = {
    enable = true;
    font.name = "Zed Mono Extended";
    font.size = 13.0;
    settings = {
      confirm_os_window_close = 0;
      update_check_interval = 0;
      window_padding_width = "5 10";
    };
    # Theme names ares specified in github.com/kovidgoyal/kitty-themes
    # Theme previews can be found in github.com/dexpota/kitty-themes
    theme = "Tomorrow Night Eighties";
  };
}
