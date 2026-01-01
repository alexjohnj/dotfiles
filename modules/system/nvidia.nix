{ pkgs, config, ... }:
{
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware = {
    nvidia = {
      open = true;
      modesetting.enable = true;
    };

    graphics = {
      enable = true;
      extraPackages = [ pkgs.nvidia-vaapi-driver ];
    };
  };

  environment.variables = {
    GBM_BACKEND = "nvidia-drm";
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    ELECTRON_OZONE_PLATFORM_HINT = "auto";
  };
}
