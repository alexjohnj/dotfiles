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

    # Flatpak forwards the host's /usr/share/icons into the sandbox. Mount the
    # system icons there; resolve-symlinks lets the sandbox follow the Nix
    # store symlinks.
    # https://wiki.nixos.org/wiki/Fixes_for_non-Nix_applications
    system.fsPackages = [ pkgs.bindfs ];
    fileSystems."/usr/share/icons" = {
      device = "/run/current-system/sw/share/icons";
      fsType = "fuse.bindfs";
      options = [
        "ro"
        "x-gvfs-hide"
        "resolve-symlinks"
      ];
    };

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
