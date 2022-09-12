{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    ./hardware.nix
    ./keychron-fix.nix
  ];

  # Configure networking
  networking = {
    hostName = "davids-desktop";
    networkmanager.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.enp8s0.useDHCP = true;
    interfaces.wlp9s0.useDHCP = true;
  };

  # Enable Steam
  programs.steam.enable = true;

  # Multiple monitor configuration
  services.xserver.xrandrHeads = [
    {
      output = "DisplayPort-2";
      monitorConfig = ''
        Modeline "1920x1080_144.00"  325.080  1920 1944 1976 2056  1080 1083 1088 1098 +hsync +vsync
        Option   "PreferredMode" "1920x1080_144.00"
        Option   "Rotate" "left"
      '';

    }
    {
      output = "DisplayPort-1";
      primary = true;
      monitorConfig = ''
        Modeline "2560x1440_144.00"  808.75  2560 2792 3072 3584  1440 1443 1448 1568 -hsync +vsync
        Option   "PreferredMode" "2560x1440_144.00"
        Option   "Position" "1080 300"
      '';
    }
  ];

  # Link xorg.conf for easier debugging
  services.xserver.exportConfiguration = true;
}
