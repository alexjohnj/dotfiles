{
  llm-agents,
  lib,
  pkgs,
  ...
}:
let
  statusLineScript = pkgs.writeShellApplication {
    name = "claude-statusline";
    runtimeInputs = [
      pkgs.jq
      pkgs.git
    ];
    text = builtins.readFile ./statusline.sh;
  };
in
{
  programs.claude-code = {
    enable = true;
    package = llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
    context = ./memory.md;
    skills = ./skills;
    settings = {
      tui = "fullscreen";
      showThinkingSummaries = true;
      awaySummaryEnabled = false;
      showClearContextOnPlanAccept = true;
      skipAutoPermissionPrompt = true;

      attribution = {
        commit = "";
        pr = "";
      };

      statusLine = {
        type = "command";
        command = "${statusLineScript}/bin/claude-statusline";
      };

      permissions = {
        defaultMode = "auto";
      };

      sandbox = {
        enabled = true;
        allowUnsandboxedCommands = false;
        dangerouslyDisableSandbox = false;
      };

      hooks = {
        SessionStart = [
          {
            hooks = [
              {
                type = "command";
                command = "${lib.getExe pkgs.direnv} export bash > $CLAUDE_ENV_FILE";
              }
            ];
          }
        ];

        CwdChanged = [
          {
            hooks = [
              {
                type = "command";
                command = "${lib.getExe pkgs.direnv} export bash > $CLAUDE_ENV_FILE";
              }
            ];
          }
        ];
      };
    };
  };
}
