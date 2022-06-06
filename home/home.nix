{ config, pkgs, ... }:

{
  # =================
  # Misc user config
  # =================
  home.username = "david";
  home.homeDirectory = "/home/david";
  home.sessionVariables = {
    EDITOR = "code";
  };

  # ========
  # X config
  # ========
  xsession = {
    enable = true;
    # Enable xmonad and point it at config
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad-config/xmonad.hs;
    };
  };

  # ===============================================================
  # Always-present user packages
  # Remember, per-project packages should be installed per-project!
  # ===============================================================
  home.packages = with pkgs; [
    google-chrome
    discord
    spotify
    element-desktop

    zip
    unzip
    file
    dig
    neofetch
    parted

    gnupg
    pinentry_qt
    pavucontrol
    playerctl
  ];

  # TODO: Convert alacritty config to a nix expression so it can be set using `programs.alacritty.settings`
  xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yaml;

  # ==============
  # Program config
  # ==============
  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;

      oh-my-zsh = {
        enable = true;
        custom = "${./zsh-custom}";
        theme = "panda-lambda";
      };
    };

    alacritty.enable = true;

    rofi.enable = true;

    vscode = {
      enable = true;
      mutableExtensionsDir = false;

      extensions =
        let
          packagedExtensions = with pkgs.vscode-extensions; [
            eamodio.gitlens
            jnoortheen.nix-ide
          ];

          # For packages that aren't in/we don't want to pull from nixpkgs (probably because they're out of date)
          # Can use `./nixpkgs/pkgs/applications/editors/vscode/extensions/update_installed_exts.sh` (See https://nixos.wiki/wiki/Visual_Studio_Code ("Managing Extensions" section))
          # Or pull info from the vscode marketplace page and use pkgs.lib.fakeHash to get the hash
          unpackagedExtensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            # Out of date in nixpkgs
            {
              name = "haskell";
              publisher = "haskell";
              version = "2.2.0";
              sha256 = "0qgp93m5d5kz7bxlnvlshcd8ms5ag48nk5hb37x02giqcavg4qv0";
            }
            # Out of date in nixpkgs
            {
              name = "language-haskell";
              publisher = "justusadam";
              version = "3.6.0";
              sha256 = "115y86w6n2bi33g1xh6ipz92jz5797d3d00mr4k8dv5fz76d35dd";
            }
            # Not in nixpkgs
            {
              name = "intellij-idea-keybindings";
              publisher = "k--kato";
              version = "1.5.1";
              sha256 = "sha256-X+q43p455J9SHBEvin1Umr4UfQVCI8vnIkoH5/vUUJs=";
            }
          ];
        in
        packagedExtensions ++ unpackagedExtensions;
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
        init.defaultBranch = "main";
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

    command-not-found.enable = true;

    exa = {
      enable = true;
      enableAliases = true;
    };

    htop.enable = true;

    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  # ==============
  # Service config
  # ==============
  services = {
    flameshot.enable = true;

    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };

    polybar = {
      enable = true;
      package = pkgs.polybar.override {
        pulseSupport = true;
      };
      script = "polybar default &";
      config = ./polybar.ini;
    };

    playerctld.enable = true;
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
