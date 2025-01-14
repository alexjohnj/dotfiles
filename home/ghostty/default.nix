{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.alexj.ghostty;
in
{
  options.alexj.ghostty = {
    enable = lib.mkEnableOption "ghostty";
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      # The macOS build is broken :(
      enable = true;
      enableFishIntegration = true;
      installBatSyntax = true;
    };

    xdg.configFile."ghostty/config".source = ./ghostty.conf;
  };
}
