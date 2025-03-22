{
  appimageTools,
  fetchurl,
  pkgs,
}:
let
  appname = "OrcaSlicer";
  pname = "orca-slicer";
  version = "2.3.0";
  src = fetchurl {
    url = "https://github.com/SoftFever/OrcaSlicer/releases/download/v2.3.0/OrcaSlicer_Linux_AppImage_V2.3.0.AppImage";
    hash = "sha256-cwediOw28GFdt5GdAKom/jAeNIum4FGGKnz8QEAVDAM=";
  };
  appimageContents = appimageTools.extractType2 { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraPkgs = pkgs: [
    pkgs.webkitgtk
    pkgs.glxinfo # Orca uses glxinfo to activate some NVIDIA specific performance fixes.
    pkgs.mesa # It needs mesa for said NVIDIA fixes.
  ];

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/OrcaSlicer.desktop $out/share/applications/${appname}.desktop
    install -m 444 -D ${appimageContents}/OrcaSlicer.png $out/share/icons/hicolor/192x192/apps/OrcaSlicer.png

    substituteInPlace $out/share/applications/${appname}.desktop \
      --replace 'Exec=AppRun' 'Exec=${pname}'
  '';
}
