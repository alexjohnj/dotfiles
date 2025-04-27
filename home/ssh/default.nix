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
        hostname = "munchlax.pokenode.net";
        user = "root";
      };

      "router" = {
        hostname = "192.168.1.1";
        user = "root";
      };

      "pibox" = {
        hostname = "pibox.home.arpa";
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

      "remarkable" = {
        hostname = "rm.pokenode.net";
        user = "root";
      };

    };
  };
}
