{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    git-lfs
    git-absorb
  ];

  programs.git.enable = true;

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };

  xdg.configFile = {
    "git/config".source = ./gitconfig;
    "git/ignore".source = ./gitignore;
    "git/credential".text = ''
      [credential]
        helper = ${
          if pkgs.stdenv.hostPlatform.isDarwin then
            "osxkeychain"
          else
            "${pkgs.gitFull}/bin/git-credential-libsecret"
        }
    '';
  };
}
