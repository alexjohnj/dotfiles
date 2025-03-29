{ ... }:
{
  # Ensure the control path directory exists.
  home.file.".ssh/ctrl/.keep".text = "";

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "~/.ssh/ctrl/%r@%h:%p";
    controlPersist = "10m";

    hashKnownHosts = true;
    addKeysToAgent = "yes";

    matchBlocks = {
      "munchlax" = {
        hostname = "munchlax.home.arpa";
        user = "root";
      };

      "router" = {
        hostname = "192.168.1.1";
        user = "root";
      };

      "pibox" = {
        hostname = "192.168.1.200";
        user = "alex";
      };

      "v0" = {
        hostname = "v0.home.arpa";
        user = "pi";
      };

      "v2" = {
        hostname = "v2.home.arpa";
        user = "biqu";
      };
    };
  };
}
