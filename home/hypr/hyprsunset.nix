{ pkgs, lib, ... }:
let
  temperature = "4500";
  startHour = "18";
  hyprsunset = pkgs.hyprsunset;
  # Script from MichaelBradley/dotfiles
  script = pkgs.writeShellScriptBin "hyprsunset" ''
    #!${pkgs.runtimeShell}
    START_HOUR='${startHour}'
    END_HOUR='6'
    CURRENT_HOUR="$(date '+%H')"

    if [ "$START_HOUR" -le "$CURRENT_HOUR" ]; then
      END_EPOCH="$(date --date="tomorrow $END_HOUR" '+%s')"
    elif [ "$CURRENT_HOUR" -lt "$END_HOUR" ]; then
      END_EPOCH="$(date --date="$END_HOUR" '+%s')"
    else
      exit 0
    fi

    CURRENT_EPOCH="$(date '+%s')"
    SECONDS_REMAINING="$((END_EPOCH - CURRENT_EPOCH))"

    timeout "$SECONDS_REMAINING" ${lib.getExe hyprsunset} -t ${temperature}
  '';
in
{
  home.packages = [ hyprsunset ];

  systemd.user = {
    services.hyprsunset = {
      Unit = {
        Description = "hyprsunset";
        After = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = lib.getExe script;
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    timers.hyprsunset = {
      Unit = {
        Description = "Check if hyprsunset needs activating";
        After = [ "graphical-session.target" ];
      };
      Timer = {
        OnCalendar = [
          "*-*-* *:00:02"
        ];
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}
