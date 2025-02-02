{ appimageTools, fetchurl }:
let
  appname = "PrusaSlicer";
  pname = "prusa-slicer";
  version = "2.8.0";
  src = fetchurl {
    url = "https://github.com/prusa3d/PrusaSlicer/releases/download/version_2.8.0/PrusaSlicer-2.8.0+linux-x64-GTK3-202406270929.AppImage";
    hash = "sha256-4BGCU3lDwoKV4SisXtBEcj9yNA/EHZMZ3MjCfcCMPmw=";
  };
  appimageContents = appimageTools.extractType2 { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraPkgs = pkgs: [
    pkgs.webkitgtk
    pkgs.libsoup_2_4
  ];

  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/PrusaSlicer.desktop $out/share/applications/${appname}.desktop
    install -m 444 -D ${appimageContents}/PrusaSlicer.png $out/share/icons/hicolor/192x192/apps/PrusaSlicer.png

    substituteInPlace $out/share/applications/${appname}.desktop \
      --replace 'Exec=AppRun' 'Exec=${pname}'
  '';
}
