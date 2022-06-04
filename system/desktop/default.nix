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
}
