{ appimageTools, fetchurl }:
let
  appname = "SuperSlicer";
  pname = "super-slicer";
  version = "2.5.59.13";
  src = fetchurl {
    url = "https://github.com/supermerill/SuperSlicer/releases/download/2.5.59.13/SuperSlicer-ubuntu_20.04-${version}.AppImage";
    hash = "sha256-My37twRvwSwZNpAL4qhc4PEwMbOB7fJm+MbQjZRzDaQ=";
  };
  appimageContents = appimageTools.extractType2 { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/SuperSlicer.desktop $out/share/applications/${appname}.desktop
    install -m 444 -D ${appimageContents}/SuperSlicer.png $out/share/icons/hicolor/192x192/apps/SuperSlicer.png

    substituteInPlace $out/share/applications/${appname}.desktop \
      --replace 'Exec=AppRun' 'Exec=${pname}'
  '';
}
