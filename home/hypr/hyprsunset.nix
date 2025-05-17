{ ... }:
{
  services.hyprsunset = {
    enable = true;
    extraArgs = [
      "--identity"
    ];
    transitions = {
      sunrise = {
        calendar = "*-*-* 07:00:00";
        requests = [
          [
            "identity"
          ]
        ];
      };
      sunset = {
        calendar = "*-*-* 20:00:00";
        requests = [
          [
            "temperature"
            "4500"
          ]
        ];
      };
    };
  };
}
