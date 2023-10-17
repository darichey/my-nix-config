{ config, pkgs, ... }:

{
  # Enable flakes
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    settings.trusted-users = [ "root" "david" ];

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Configure boot loader
  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
    };
    grub = {
      devices = [ "nodev" ];
      efiSupport = true;
      enable = true;
      useOSProber = true;
    };
  };

  # Enable DBus
  services.dbus.enable = true;

  # Configure X
  services.xserver = {
    enable = true;

    libinput.enable = true;

    layout = "us";

    displayManager = {
      defaultSession = "none+xmonad";
      lightdm = {
        enable = true;
        greeter.enable = true;
      };
    };

    windowManager.xmonad.enable = true;
  };

  # Enable gvfs
  services.gvfs.enable = true;

  # Enable pipewire (https://nixos.wiki/wiki/PipeWire)
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable zsh. This has to be done at the system level to properly generate
  # zshrc and make it the default shell for the user below. 
  programs.zsh.enable = true;

  # Enable keybase here since I couldn't get the home manager service for it working
  services.keybase.enable = true;

  # Enable dconf for programs like blueman
  programs.dconf.enable = true;

  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-volman
  ];

  # Define user account
  users.users.david = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
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
  system.stateVersion = "21.11"; # Did you read the comment?
}
