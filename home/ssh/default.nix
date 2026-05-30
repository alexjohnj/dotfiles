{ ... }:
{
  # Ensure the control path directory exists.
  home.file.".ssh/ctrl/.keep".text = "";

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
      "*" = {
        HashKnownHosts = true;
        AddKeysToAgent = "yes";

        ControlMaster = "auto";
        ControlPath = "~/.ssh/ctrl/%r@%h:%p";
        ControlPersist = "10m";
      };

      "munchlax" = {
        HostName = "munchlax.pokenode.net";
        User = "root";
      };

      "pikachu" = {
        User = "alex";
        HostName = "pikachu.pokenode.net";
      };

      "router" = {
        HostName = "192.168.1.1";
        User = "root";
      };

      "pibox" = {
        HostName = "pibox.home.arpa";
        User = "alex";
      };

      "v0" = {
        HostName = "v0.home.arpa";
        User = "pi";
      };

      "v2" = {
        HostName = "v2.home.arpa";
        User = "biqu";
      };

      "remarkable" = {
        HostName = "rm.pokenode.net";
        User = "root";
      };
    };
  };
}
