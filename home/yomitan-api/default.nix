{
  config,
  pkgs,
  yomitan-api,
  ...
}:
let
  # Writable dir for the `.crowbar` PID lock and error.log that
  # yomitan_api.py writes next to itself on every run.
  stateDir = "${config.home.homeDirectory}/.local/state/yomitan-api";

  yomitanApi = pkgs.runCommand "yomitan-api" { nativeBuildInputs = [ pkgs.gnused ]; } ''
    mkdir -p "$out/bin"
    cp ${yomitan-api}/yomitan_api.py "$out/bin/yomitan_api.py"

    # Yomitan spawns this as a Firefox native messaging host, so it needs an
    # absolute shebang instead of relying on `python3` being on PATH.
    sed -i '1s|.*|#!${pkgs.python3}/bin/python3 -u|' "$out/bin/yomitan_api.py"

    # Point its state dir at somewhere writable instead of its own (read-only
    # Nix store) directory, otherwise it silently fails to start.
    substituteInPlace "$out/bin/yomitan_api.py" \
      --replace-fail \
        'script_path = os.path.realpath(os.path.dirname(__file__))' \
        'script_path = "${stateDir}"'

    chmod +x "$out/bin/yomitan_api.py"
  '';

  nativeMessagingHostsDir =
    if pkgs.stdenv.hostPlatform.isDarwin then
      "Library/Application Support/Mozilla/NativeMessagingHosts"
    else
      ".mozilla/native-messaging-hosts";
in
{
  home.file.".local/state/yomitan-api/.keep".text = "";

  home.file."${nativeMessagingHostsDir}/yomitan_api.json".text = builtins.toJSON {
    name = "yomitan_api";
    description = "Yomitan API";
    path = "${yomitanApi}/bin/yomitan_api.py";
    type = "stdio";
    allowed_extensions = [ "{6b733b82-9261-47ee-a595-2dda294a4d08}" ];
  };
}
