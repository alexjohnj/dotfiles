{ appimageTools, fetchurl }:
let
  appname = "OrcaSlicer";
  pname = "orca-slicer";
  version = "2.2.0";
  src = fetchurl {
    url = "https://github.com/SoftFever/OrcaSlicer/releases/download/v${version}/OrcaSlicer_Linux_V${version}.AppImage";
    hash = "sha256-3uqA3PXTrrOE0l8ziRAtmQ07gBFB+1Zx3S6JhmOPrZ8=";
  };
  appimageContents = appimageTools.extractType2 { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraPkgs = pkgs: [ pkgs.webkitgtk ];

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/OrcaSlicer.desktop $out/share/applications/${appname}.desktop
    install -m 444 -D ${appimageContents}/OrcaSlicer.png $out/share/icons/hicolor/192x192/apps/OrcaSlicer.png

    substituteInPlace $out/share/applications/${appname}.desktop \
      --replace 'Exec=AppRun' 'Exec=${pname}'
  '';
}
