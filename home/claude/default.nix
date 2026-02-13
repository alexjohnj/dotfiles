{ ... }:
{
  programs.claude-code = {
    enable = true;
    package = null;
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
