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
    file
    neofetch
    cabal2nix
  ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad-config/xmonad.hs;
  };

  xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yaml;

  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;

      oh-my-zsh = {
        enable = true;
        custom = "$HOME/my-nix-config/home/zsh-custom";
        theme = "panda-lambda";
      };
    };

    alacritty.enable = true;

    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        jnoortheen.nix-ide
        haskell.haskell
        justusadam.language-haskell
      ];
    };

    git = {
      enable = true;

      userName = "David Richey";
      userEmail = "darichey1@gmail.com";
      delta.enable = true;

      extraConfig = {
        core.editor = "code --wait";
        core.autocrlf = "input";
        pull.ff = "only";
        commit.gpgsign = true;
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
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
