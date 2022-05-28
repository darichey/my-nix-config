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
    dig
    haskell-language-server
    xorg.libX11
    spotify
  ];

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad-config/xmonad.hs;
    };
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

    rofi.enable = true;

    vscode = {
      enable = true;
      # extensions = with pkgs.vscode-extensions; [
      #   jnoortheen.nix-ide
      #   haskell.haskell
      #   justusadam.language-haskell
      # ];

      # Extensions in nixpkgs get out of date pretty quickly...
      # Generate this list with `./nixpkgs/pkgs/applications/editors/vscode/extensions/update_installed_exts.sh`
      # See https://nixos.wiki/wiki/Visual_Studio_Code ("Managing Extensions" section)
      extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "haskell";
          publisher = "haskell";
          version = "2.2.0";
          sha256 = "0qgp93m5d5kz7bxlnvlshcd8ms5ag48nk5hb37x02giqcavg4qv0";
        }
        {
          name = "nix-ide";
          publisher = "jnoortheen";
          version = "0.1.20";
          sha256 = "16mmivdssjky11gmih7zp99d41m09r0ii43n17d4i6xwivagi9a3";
        }
        {
          name = "language-haskell";
          publisher = "justusadam";
          version = "3.6.0";
          sha256 = "115y86w6n2bi33g1xh6ipz92jz5797d3d00mr4k8dv5fz76d35dd";
        }
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

    bat.enable = true;

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
