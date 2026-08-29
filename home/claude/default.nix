{
  llm-agents,
  mattpocock-skills,
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

  localSkills = lib.mapAttrs (name: _: ./skills + "/${name}") (builtins.readDir ./skills);

  # Selected skills from https://github.com/mattpocock/skills
  mattpocockProductivitySkills = lib.genAttrs [
    "grilling"
    "grill-me"
    "handoff"
    "teach"
    "writing-for-agents"
  ] (name: "${mattpocock-skills}/skills/productivity/${name}");

  mattpocockEngineeringSkills = lib.genAttrs [
    "ask-matt"
    "code-review"
    "codebase-design"
    "diagnosing-bugs"
    "domain-modeling"
    "grill-with-docs"
    "implement"
    "improve-codebase-architecture"
    "prototype"
    "research"
    "resolving-merge-conflicts"
    "setup-matt-pocock-skills"
    "tdd"
    "to-spec"
    "to-tickets"
    "triage"
    "wayfinder"
    "wizard"
  ] (name: "${mattpocock-skills}/skills/engineering/${name}");
in
{
  programs.claude-code = {
    enable = true;
    package = llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
    context = ./memory.md;
    skills = localSkills // mattpocockProductivitySkills // mattpocockEngineeringSkills;
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

        deny = [
          "Bash(direnv *)"
          "Bash(gh pr create:*)"
          "Bash(gh pr ready:*)"
          "Skill(plex-tools:pr)"
        ];
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
