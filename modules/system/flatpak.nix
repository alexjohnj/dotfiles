{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.modules.flatpak;
in
{
  options.modules.flatpak = {
    enable = lib.mkEnableOption "Flatpak support";

    autoUpdate = {
      enable = lib.mkEnableOption "Automatic Flatpak updates";

      onCalendar = lib.mkOption {
        type = lib.types.str;
        default = "daily";
        description = "When to run automatic updates (systemd calendar format)";
      };

      randomizedDelaySec = lib.mkOption {
        type = lib.types.str;
        default = "1h";
        description = "Random delay to spread load";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.flatpak.enable = true;

    systemd.services.flatpak-update = lib.mkIf cfg.autoUpdate.enable {
      description = "Update system flatpaks";
      path = [ pkgs.flatpak ];
      script = ''
        flatpak update --assumeyes --noninteractive --system
      '';
      serviceConfig = {
        Type = "oneshot";
        # Security hardening - runs as root but with restrictions
        PrivateTmp = true;
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
      };
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
    };

    systemd.timers.flatpak-update = lib.mkIf cfg.autoUpdate.enable {
      description = "Update system flatpaks timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.autoUpdate.onCalendar;
        Persistent = true;
        RandomizedDelaySec = cfg.autoUpdate.randomizedDelaySec;
      };
    };
  };
}
