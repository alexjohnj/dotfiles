{ config, pkgs, ... }:
let
  inherit (pkgs) lib;
  isLinux = pkgs.stdenv.hostPlatform.isLinux;

  emacsConfigDir = ./emacs.d;
  packageConfigDir = emacsConfigDir + "/package-config";
  packageConfigFiles = builtins.attrNames (
    lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".el" name) (
      builtins.readDir packageConfigDir
    )
  );
  elispFiles = [
    (emacsConfigDir + "/init.el")
  ]
  ++ map (f: packageConfigDir + "/${f}") packageConfigFiles;

  # emacsWithPackagesFromUsePackage parses this text with Nix which chokes on
  # the form-feed (^L) characters used as section separators, so strip them
  # here.
  formFeed = builtins.fromJSON ''"\f"'';
  emacsConfigText = builtins.replaceStrings [ formFeed ] [ "" ] (
    builtins.concatStringsSep "\n" (map builtins.readFile elispFiles)
  );
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = emacsConfigText;
      package = if isLinux then pkgs.emacs30-pgtk else pkgs.emacs30;
      # Every use-package block in emacs.d omits :ensure and relies on this,
      # mirroring the old `use-package-always-ensure t` behaviour. Blocks
      # that shouldn't be Nix-installed (built-ins, packages installed
      # below, locally vendored files) explicitly set `:ensure nil`.
      alwaysEnsure = true;
      extraEmacsPackages =
        epkgs: with epkgs; [
          jinx
          treesit-grammars.with-all-grammars
        ];
    };
  };

  services.emacs.enable = isLinux;

  home.packages = with pkgs; [
    # This dictionary is needed for jinx.
    hunspellDicts.en_GB-large
    # Provides some language servers used in Emacs
    vscode-langservers-extracted
    emacs-lsp-booster
  ];

  # Emacs configuration files are symlinked instead of copied to the nix
  # store so I can iterate on my Emacs configuration "frequently" without
  # having to rebuild every time something changes. Packages are installed
  # by Nix (see emacsConfigText above), so this symlink only needs to cover
  # elisp config files, not package installs.
  #
  # The downside is this breaks the purity of the flake. It'd be nice if there
  # was a way to fix this in future.
  #
  xdg.configFile."emacs" = {
    recursive = true;
    # HACK: Nix flakes are pure meaning specifying a relative path to the
    # emacs.d directory will result in it being copied to the nix store
    # still. To workaround this without having to pass the --impure flag to
    # nixos-rebuild, I have to specify an absolute path to the location of
    # emacs.d.
    #
    # See nix-community/home-manager/issues/2085
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/home/emacs/emacs.d";
  };
}
