{ pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  # Enable support for flakes.
  nix.settings = { experimental-features = "flakes nix-command"; };

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

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # Enable and configure Nvidia graphics drivers.
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
  };

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.desktopManager.gnome.enable = true;
  security.pam.services.gdm.enableGnomeKeyring = true;
  environment.gnome.excludePackages = (with pkgs; [ gnome-tour gnome-console ])
    ++ (with pkgs.gnome; [ cheese epiphany geary gnome-music totem ]);

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

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "colemak";
    excludePackages = [ pkgs.xterm ];
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable the fish shell
  programs.fish.enable = true;

  users.users.alex = {
    isNormalUser = true;
    description = "Alex Jackson";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    packages = with pkgs; [
      (aspellWithDicts (dicts: [ dicts.en ]))
      _1password-gui
      bat
      direnv
      discord
      emacs29-gtk3
      f3d
      fd
      firefox
      kitty
      nil # nix lsp server
      nixfmt # "nix fmt" is built in but Emacs is looking for "nixfmt" which isn't.
      overpass
      ripgrep
    ];
  };

  # Enable Syncthing
  services.syncthing = {
    enable = true;
    user = "alex";
    dataDir = "/home/alex";
    configDir = "/home/alex/.config/syncthing";
    openDefaultPorts = true;
  };

  services.flatpak.enable = true;

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "alex";

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = (with pkgs; [ git htop tree vim ])
    ++ (with pkgs.gnomeExtensions; [ dash-to-dock blur-my-shell ]);

  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
