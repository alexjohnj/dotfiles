{
  pkgs,
  lib,
  config,
  secrets,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/system/nvidia.nix
  ];

  nix = {
    settings = {
      experimental-features = "flakes nix-command";
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  age = {
    identityPaths = [ "${config.users.users.alex.home}/.ssh/id_ed25519" ];

    secrets = {
      "restic/env" = {
        file = secrets.files.restic."env.age";
        owner = "alex";
      };

      "restic/password" = {
        file = secrets.files.restic."password.age";
        owner = "alex";
      };

      "restic/repo/b2" = {
        file = secrets.files.restic.repo."b2.age";
        owner = "alex";
      };
    };
  };

  nixpkgs.config.allowUnfree = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-91e75985-10c9-42bb-8657-5b76c1b1dc9e".device =
    "/dev/disk/by-uuid/91e75985-10c9-42bb-8657-5b76c1b1dc9e";

  # Networking
  networking.hostName = "pikachu";
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_GB.UTF-8";
    LC_IDENTIFICATION = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
    LC_MONETARY = "en_GB.UTF-8";
    LC_NAME = "en_GB.UTF-8";
    LC_NUMERIC = "en_GB.UTF-8";
    LC_PAPER = "en_GB.UTF-8";
    LC_TELEPHONE = "en_GB.UTF-8";
    LC_TIME = "en_GB.UTF-8";
  };

  # Desktop configuration
  services.xserver = {
    enable = true;

    xkb = {
      layout = "us";
      variant = "colemak";
    };

    excludePackages = [ pkgs.xterm ];
  };

  # Use X keyboard layout in the console (specifically the disk decryption
  # password prompt).
  console.useXkbConfig = true;

  # Support AppImages somehow?
  # This is kind'a cool.
  boot.binfmt.registrations.appimage = {
    wrapInterpreterInShell = false;
    interpreter = "${pkgs.appimage-run}/bin/appimage-run";
    recognitionType = "magic";
    offset = 0;
    mask = "\\xff\\xff\\xff\\xff\\x00\\x00\\x00\\x00\\xff\\xff\\xff";
    magicOrExtension = "\\x7fELF....AI\\x02";
  };

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  programs = {
    # Fish must be enabled here and in home-manager
    fish.enable = true;

    steam.enable = true;

    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };

    uwsm.enable = true;
    hyprland = {
      enable = true;
      withUWSM = true;
    };
    hyprlock.enable = true;
  };

  # Enables support for volumes in thunar
  services.gvfs.enable = true;

  # Display Manager
  services.greetd = {
    enable = true;
    settings = rec {
      default_session = {
        command = "${lib.getExe config.programs.uwsm.package} start hyprland-uwsm.desktop";
        user = "alex";
      };
      initial_session = default_session;
    };
  };

  environment.systemPackages = with pkgs; [
    pavucontrol
    xarchiver
  ];

  services.playerctld.enable = true;

  users.users.alex = {
    isNormalUser = true;
    description = "Alex Jackson";
    extraGroups = [
      "networkmanager"
      "wheel"
      "audio"
      "docker"
    ];
    shell = pkgs.fish;
  };

  # Enable Syncthing
  services.syncthing = {
    enable = true;
    user = "alex";
    dataDir = "/home/alex";
    configDir = "/home/alex/.config/syncthing";
    openDefaultPorts = true;
  };

  # Backups with restic
  services.restic.backups =
    let
      homeDir = config.users.users.alex.home;
      common = {
        exclude = [
          "**/.cache"
          "**/Cache"
          "**/cache"
          "**/__pycache__"
          "**/node_modules"
          "${homeDir}/.cargo"
          "${homeDir}/.npm"
          "${homeDir}/.vscode"
          "${homeDir}/.local/share"
          "${homeDir}/Applications"
        ];

        extraBackupArgs = [
          "--exclude-caches"
        ];

        initialize = true;
        user = "alex";
        environmentFile = config.age.secrets."restic/env".path;
        passwordFile = config.age.secrets."restic/password".path;
      };
    in
    {
      munchlax = common // {
        repository = "rest:http://munchlax.home.arpa:8020";
        timerConfig = {
          OnCalendar = "hourly";
          Persistent = true;
        };
        paths = [ homeDir ];
        pruneOpts = [
          "--keep-hourly 24"
          "--keep-daily 7"
          "--keep-weekly 5"
          "--keep-monthly 12"
        ];
      };

      b2 = common // {
        repositoryFile = config.age.secrets."restic/repo/b2".path;
        timerConfig = {
          OnCalendar = "daily";
          Persistent = true;
        };
        paths = [
          "${homeDir}/Desktop"
          "${homeDir}/Documents"
          "${homeDir}/Pictures"
          "${homeDir}/finance"
          "${homeDir}/src"
        ];
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 5"
          "--keep-monthly 12"
        ];
      };
    };

  services.flatpak.enable = true;

  services.tailscale = {
    enable = true;
    extraSetFlags = [
      "--operator"
      "alex"
    ];
  };

  networking.firewall.enable = true;

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
