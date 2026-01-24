{
  pkgs,
  lib,
  secrets,
  config,
  ...
}:
{
  age.secrets = {
    hashedPasswordFile.file = secrets.files.host.pibox."hashedPasswordFile.age";
  };

  # Speed up builds by disabling compression.
  sdImage.compressImage = false;
  boot.supportedFilesystems.zfs = lib.mkForce false;

  time.timeZone = "Europe/London";
  services.ntp.enable = true;

  # Reduce SD card wear.
  boot.tmp.useTmpfs = true;
  fileSystems."/".options = [ "noatime" ];
  services.journald.extraConfig = ''
    Storage=volatile
    RuntimeMaxUse=64M
  '';

  networking = {
    hostName = "pibox";

    firewall = {
      enable = true;
    };
  };

  nix.settings = {
    trusted-users = [ "alex" ];
    experimental-features = "flakes nix-command";
  };

  # Keep only the current + 1 previous generation to avoid filling the SD card.
  boot.loader.generic-extlinux-compatible.configurationLimit = 2;
  systemd.services.nixos-prune-generations = {
    description = "Delete old NixOS generations and collect garbage";
    serviceConfig.Type = "oneshot";
    script = ''
      ${pkgs.nix}/bin/nix-env -p /nix/var/nix/profiles/system --delete-generations +2
      ${pkgs.nix}/bin/nix-collect-garbage
    '';
  };
  systemd.timers.nixos-prune-generations = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };

  environment.systemPackages = with pkgs; [
    libraspberrypi
    vim
  ];

  programs.fish.enable = true;

  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "server";
      extraSetFlags = [
        "--advertise-exit-node"
        "--advertise-routes=192.168.1.1/32,192.168.1.2/32"
      ];
    };

    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        AllowUsers = [ "alex" ];
      };
    };
  };

  security.sudo.execWheelOnly = true;

  users = {
    mutableUsers = false;

    users.alex = {
      hashedPasswordFile = config.age.secrets.hashedPasswordFile.path;
      shell = pkgs.fish;
      isNormalUser = true;
      description = "Alex Jackson";
      extraGroups = [
        "wheel"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFrNwXSpOYqL0Sj41iTor+R5yngAzV1IUqv3PZIKnjvl alex@pop-os"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIr9c9JzaN5dNyoTuP63ne3MqfCWRqdulB4xIEFMf36u alex@alexj.org"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJd08Z4mwUiOheQ7YKxomaSiXI7tnPqQA6JOmE/xNqeMocY162IFFZTzTgxY1YnMXsTiFO2eM0C3pd+HM67dlmL5cIALFQZDsGEvW5F89rpuL9tVBgnDWP+EIjLlzNLH0BT8I9vEzY7bnMROg9bvhOUrSVglcj1UO0Wf4i/Q9rFr/O4I1HfIDnFTC6gmW8c+ayIVKEQLAxR0WV4KpWkg5TY6KovuUUQtRmp8nRe2wWXPBA733Jjde2pRO16NjrRTh6Ig6f/lbI77AL2EJdU76AY7HjySNA8TWvYe5tg4XmyHIdy4CLwgo8KdXKZAMeWk0s/xFNl+tU0uuSHTwYTBPwk4anSJBGH3fCaB0tW/k/9WUMjzcdZFhjmZ51GyDr/pEmt0SIvxPzatIDU8s9RMLNkWPlFlL7oihEJQjxXOQvR/qAYuH/hbUih0dPPrtSmOffXMJDDqXM2rNzCE3HHrdgHHMXuiknsCrIlOLDwb2NWHNFANm6lbRxYT+WefN5tZk= alex@mocha-windows"
      ];
    };
  };

  # Fish enables this for completions but it's incredibly slow to generate when
  # cross-compiling.
  documentation.man.cache.enable = false;

  system.stateVersion = "24.11";
}
