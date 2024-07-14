{ appimageTools, fetchurl }:
let
  appname = "OrcaSlicer";
  pname = "orca-slicer";
  version = "2.1.1";
  src = fetchurl {
    url = "https://github.com/SoftFever/OrcaSlicer/releases/download/v2.1.1/OrcaSlicer_Linux_V${version}.AppImage";
    hash = "sha256-kvM1rBGEJhjRqQt3a8+I0o4ahB1Uc9qB+4PzhYoNQdM=";
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
