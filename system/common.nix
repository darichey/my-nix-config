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

  # Note that we are using wayland, but services.xserver is named poorly: https://github.com/NixOS/nixpkgs/issues/94799
  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      # TODO: theme = ...
    };
  };
  programs.hyprland.enable = true;

  # Enable zsh. This has to be done at the system level to properly generate
  # zshrc and make it the default shell for the user below. 
  programs.zsh.enable = true;

  # Enable keybase here since I couldn't get the home manager service for it working
  services.keybase.enable = true;

  # Enable dconf for programs like blueman and virt-manager
  programs.dconf.enable = true;

  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-volman
  ];

  programs.steam.enable = true;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark;
  services.udev = {
    extraRules = ''
      # Allow wireshark to monitor usb traffic 
      SUBSYSTEM=="usbmon", GROUP="wireshark", MODE="0640"
      # Allow everyone to read/write MSI MAG 323UPF KVM/USB hub
      KERNEL=="hidraw*", ATTRS{idVendor}=="1462", ATTRS{idProduct}=="3fa4", MODE="0666"
    '';
  };

  # Define user account
  users.users.david = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "libvirtd" "wireshark" ];
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
