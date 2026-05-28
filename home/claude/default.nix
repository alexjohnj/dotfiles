{ llm-agents, pkgs, ... }:
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
    package = llm-agents.packages.${pkgs.system}.claude-code;
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
    };
  };
}
