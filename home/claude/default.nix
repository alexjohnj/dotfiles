{ llm-agents, pkgs, ... }:
{
  programs.claude-code = {
    enable = true;
    package = llm-agents.packages.${pkgs.system}.claude-code;
    memory.source = ./memory.md;
    skillsDir = ./skills;
    settings = {
      attribution = {
        commit = "";
        pr = "";
      };
    };
  };
}
