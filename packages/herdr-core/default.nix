{
  lib,
  stdenvNoCC,
  fetchurl,
}:
let
  version = "0.1.14";
  sources = {
    x86_64-linux = {
      target = "linux-x86_64";
      hash = "sha256-6dur3q0M4M495RtvVMyVSe/tu0Gy+ESLaTKrDRRCND4=";
    };
    aarch64-linux = {
      target = "linux-aarch64";
      hash = "sha256-Gc72ouUvFMIsS28/2vGkYtLa/HjosD+rv61bBjR3o3A=";
    };
  };
  source =
    sources.${stdenvNoCC.hostPlatform.system}
      or (throw "herdr-core: unsupported system ${stdenvNoCC.hostPlatform.system}");
in
stdenvNoCC.mkDerivation {
  pname = "herdr-core";
  inherit version;

  src = fetchurl {
    url = "https://releases.herdr.dev/core/${version}/herdr-core-${source.target}.tar.gz";
    inherit (source) hash;
  };

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    install -Dm755 herdr-core $out/bin/herdr-core
    ln -s herdr-core $out/bin/hc
    runHook postInstall
  '';

  meta = {
    description = "Terminals for coding agents";
    homepage = "https://github.com/herdrdev/herdr-core";
    license = lib.licenses.asl20;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = builtins.attrNames sources;
    mainProgram = "hc";
  };
}
