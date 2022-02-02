{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Enable flakes
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Configure boot loader
  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    grub = {
      devices = [ "nodev" ];
      efiSupport = true;
      enable = true;
      version = 2;
      useOSProber = true;
    };
  };

  # Configure networking
  networking = {
    hostName = "davids-laptop";
    networkmanager.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.wlp59s0.useDHCP = true;
  };
  
  # Enable X and Plasma
  services.xserver = {
    enable = true;
    desktopManager.plasma5.enable = true;
    libinput.enable = true;
    layout = "us";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  
  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Enable zsh. This has to be done at the system level to properly generate
  # zshrc and make it the default shell for the user below. 
  programs.zsh.enable = true;

  # Define user account
  users.users.david = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    initialPassword = "password";
    shell = pkgs.zsh;
  };

  time.timeZone = "America/Chicago";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
