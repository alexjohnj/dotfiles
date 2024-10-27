{ config, pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    font.name = "Zed Mono Extended";
    font.size = 13.0;
    settings = {
      # See https://sw.kovidgoyal.net/kitty/conf/
      confirm_os_window_close = 0;
      update_check_interval = 0;
      window_padding_width = "5 10";
      cursor_blink_interval = 0;
      tab_bar_style = "powerline";
      tab_powerline_style = "round";
    };
    keybindings = {
      "kitty_mod+t" = "new_tab_with_cwd";
      "cmd+t" = "new_tab_with_cwd";
    };
    # Theme names ares specified in github.com/kovidgoyal/kitty-themes
    # Theme previews can be found in github.com/dexpota/kitty-themes
    themeFile = "Modus_Vivendi";
  };
}
