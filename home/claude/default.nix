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
    memory.source = ./memory.md;
    skillsDir = ./skills;
    settings = {
      tui = "fullscreen";
      attribution = {
        commit = "";
        pr = "";
      };
      statusLine = {
        type = "command";
        command = "${statusLineScript}/bin/claude-statusline";
      };
      sandbox = {
        enabled = true;
        allowUnsandboxedCommands = false;
        dangerouslyDisableSandbox = false;
      };
    };
  };
}
