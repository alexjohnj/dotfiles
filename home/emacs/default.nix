{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages =
      epkgs: with epkgs; [
        jinx
        treesit-grammars.with-all-grammars
      ];
  };

  services.emacs.enable = pkgs.stdenv.hostPlatform.isLinux;

  # This dictionary is needed for jinx.
  home.packages = [ pkgs.hunspellDicts.en_GB-large ];

  # Emacs configuration files are symlinked instead of copied to the nix
  # store. There's a few reasons for this:
  #
  # 1. I iterate on my Emacs configuration "frequently" so having to rebuild
  # every time something changes is annoying.
  #
  # 2. I use straight to manage packages in Emacs which is "reproducible" if you
  # squint hard enough.
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
