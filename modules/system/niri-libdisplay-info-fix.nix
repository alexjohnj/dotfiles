# Workaround for a nixpkgs regression: nixpkgs bumped the default
# `libdisplay-info` to 0.4.0, but niri 26.04's vendored libdisplay-info-sys
# crate requires < 0.4.0, breaking the build.
#
# The assertion fails loudly once nixpkgs is bumped past the upstream fix and
# niri no longer needs this override, so it can be deleted.
{ lib, ... }:
{
  nixpkgs.overlays = [
    (final: prev: {
      niri =
        if lib.elem prev.libdisplay-info prev.niri.buildInputs then
          prev.niri.override { libdisplay-info = prev.libdisplay-info_0_2; }
        else
          throw ''
            niri no longer depends on the default libdisplay-info package, so
            the libdisplay-info_0_2 override in
            modules/system/niri-libdisplay-info-fix.nix is likely no longer
            needed. Delete this module and its import in
            hosts/pikachu/configuration.nix.
          '';
    })
  ];
}
