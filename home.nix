{ config, pkgs, ...}:

{
  home.username = "david";
  home.homeDirectory = "/home/david";
  
  home.sessionVariables = {
      EDITOR = "code";
  };
  
  home.packages = with pkgs; [
      google-chrome
      discord
      zip
      unzip
      gnupg
      pinentry_qt
  ];

  programs = {
    zsh.enable = true;

    vscode = {
        enable = true;
        extensions = with pkgs.vscode-extensions; [
          jnoortheen.nix-ide
        ];
    };

    git = {
        enable = true;
        userName = "David Richey";
        userEmail = "darichey1@gmail.com";
    };

    ssh.enable = true;

    gpg.enable = true;

    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  services = {
    flameshot.enable = true;
    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";
}
