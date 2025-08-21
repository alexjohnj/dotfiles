{ ... }:
{
  services.hyprsunset = {
    enable = true;
    settings = {
      profile = [
        {
          time = "07:00";
          identity = true;
        }
        {
          time = "19:15";
          temperature = 4500;
        }
      ];
    };
  };
}
