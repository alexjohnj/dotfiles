{
  config,
  pkgs,
  llm-agents,
  ...
}:
let
  # nixpkgs lags upstream; the version and src are let-bound in the package,
  # so swap the inputs that use them.
  unsloth-desktop =
    let
      version = "0.1.922-beta";
    in
    pkgs.unsloth-desktop.override {
      fetchurl =
        _:
        pkgs.fetchurl {
          url = "https://github.com/unslothai/unsloth/releases/download/v${version}/Unsloth-Desktop-Ubuntu.deb";
          hash = "sha256-juTXGs549O9StWWW6s+fY8ME4d0Tt5tn2P/QYfI2z54=";
        };
      buildFHSEnv = args: pkgs.buildFHSEnv (args // { inherit version; });
    };
in
{
  home.packages =
    with pkgs;
    [
      (callPackage ../packages/super-slicer/default.nix { })
      anki
      docker-credential-helpers
      google-chrome
      handy
      inter
      libsecret
      noto-fonts
      orca-slicer
      overpass
      python3
      wl-clipboard-rs
      wtype
    ]
    ++ [ unsloth-desktop ]
    ++ (with llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      claude-desktop
    ]);

  home.file.".config/containers/auth.json".text = builtins.toJSON {
    credHelpers = {
      "registry.gitlab.com" = "secretservice";
    };
  };

  # podman/docker default to $XDG_RUNTIME_DIR/containers/auth.json (tmpfs, wiped
  # on reboot), which bypasses the credHelpers config above. Point at the
  # persistent file instead so logins actually go through secretservice.
  home.sessionVariables.REGISTRY_AUTH_FILE = "${config.xdg.configHome}/containers/auth.json";

  programs.firefox = {
    enable = true;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
  };

  # Shadows the flatpak's desktop entry (XDG_DATA_HOME takes precedence over
  # flatpak exports). 1Password writes the clipboard via X11, which
  # xwayland-satellite can't hand to Wayland while 1Password's own Wayland
  # window holds focus, so run the whole app under X11.
  xdg.dataFile."applications/com.onepassword.OnePassword.desktop".text = ''
    [Desktop Entry]
    Name=1Password
    Exec=flatpak run --branch=stable --arch=x86_64 --command=1password --file-forwarding com.onepassword.OnePassword --ozone-platform=x11 @@u %U @@
    Terminal=false
    Type=Application
    Icon=com.onepassword.OnePassword
    StartupWMClass=1Password
    Comment=Password manager and secure wallet
    MimeType=x-scheme-handler/onepassword;
    Categories=Office;
  '';

  programs.rex.enable = true;
  services.rex.enable = true;

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-mozc-ut # Japanese
        fcitx5-gtk
      ];
    };
  };

  imports = [
    ./cursor-theme
    ./gtk
    ./niri
    ./noctalia
  ];
}
