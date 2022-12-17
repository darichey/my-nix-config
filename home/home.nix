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

  # Copy wallpapers into home dir
  home.file.".wallpapers".source = ../wallpapers;

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
    peek
    slack
    gimp
    obs-studio
    vlc
    jetbrains.idea-community
    jdk
    comma
    gparted

    keybase
    kbfs # needed for git-remote-keybase

    cachix

    zip
    unzip
    file
    dig
    neofetch
    parted
    xclip
    hyperfine

    gnupg
    pinentry_qt
    pavucontrol
    playerctl
    brightnessctl

    at-spi2-core # https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424

    prismlauncher
  ];

  xdg = {
    # TODO: Convert alacritty config to a nix expression so it can be set using `programs.alacritty.settings`
    configFile."alacritty/alacritty.yml".source = ./alacritty.yaml;

    mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = [ "thunar.desktop" ];
      };
    };
  };

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

    rofi = {
      enable = true;
      plugins = [ pkgs.rofi-calc ];
    };

    vscode = {
      enable = true;
      mutableExtensionsDir = false;
      userSettings = {
        "editor.minimap.enabled" = false; # Disable minimap
        "haskell.manageHLS" = "PATH"; # Don't allow haskell extension to install things
        "files.associations" = {
          "*.nix" = "nix"; # Associate .nix files to nix extension
          "*.y" = "yacc";
        };
        "nix.enableLanguageServer" = true; # Enable using LSP in nix extension
        "nix.serverPath" = pkgs.lib.getExe pkgs.rnix-lsp; # Point to rnix-lsp executable in nix store
        "workbench.editor.revealIfOpen" = true;
        "terminal.integrated.scrollback" = 10000;
        "vscode-dhall-lsp-server.executable" = pkgs.lib.getExe pkgs.dhall-lsp-server;
        "tailwindCSS.includeLanguages" = {
          "purescript" = "html";
        };
        "tailwindCSS.experimental.classRegex" = [
          "twclass ['\"](.+?)['\"]"
        ];
        "purescript.formatter" = "purs-tidy";
        "terminal.integrated.shellIntegration.enabled" = false; # work around https://github.com/microsoft/vscode/issues/158052
      };

      extensions =
        let
          packagedExtensions = with pkgs.vscode-extensions; [
            eamodio.gitlens
            jnoortheen.nix-ide
            rust-lang.rust-analyzer
            bungcip.better-toml
            daohong-emilio.yash
            ms-vscode.cpptools
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
            # {
            #   name = "intellij-idea-keybindings";
            #   publisher = "k--kato";
            #   version = "1.5.1";
            #   sha256 = "sha256-X+q43p455J9SHBEvin1Umr4UfQVCI8vnIkoH5/vUUJs=";
            # }
            {
              name = "direnv";
              publisher = "mkhl";
              version = "0.6.1";
              sha256 = "sha256-5/Tqpn/7byl+z2ATflgKV1+rhdqj+XMEZNbGwDmGwLQ=";
            }
            {
              name = "vscode-position";
              publisher = "jtr";
              version = "1.1.2";
              sha256 = "sha256-8FZTC26xtFe+2ObT/2UO/qmYipszexgGTJRZNFy3qu8=";
            }
            {
              name = "language-purescript";
              publisher = "nwolverson";
              version = "0.2.8";
              sha256 = "sha256-2uOwCHvnlQQM8s8n7dtvIaMgpW8ROeoUraM02rncH9o=";
            }
            {
              name = "ide-purescript";
              publisher = "nwolverson";
              version = "0.25.12";
              sha256 = "sha256-tgZ0PnWrSDBNKBB5bKH/Fmq6UVNSRYZ8HJdzFDgxILk=";
            }
            {
              name = "dhall-lang";
              publisher = "dhall";
              version = "0.0.4";
              sha256 = "sha256-7vYQ3To2hIismo9IQWRWwKsu4lXZUh0Or89WDLMmQGk=";
            }
            {
              name = "vscode-dhall-lsp-server";
              publisher = "dhall";
              version = "0.0.4";
              sha256 = "sha256-WopWzMCtiiLrx3pHNiDMZYFdjS359vu3T+6uI5A+Nv4=";
            }
            {
              name = "vscode-tailwindcss";
              publisher = "bradlc";
              version = "0.8.6";
              sha256 = "sha256-v15KuD3eYFCsrworCJ1SZAMkyZKztAwWKmfwmbirleI=";
            }
            {
              name = "beancount";
              publisher = "Lencerf";
              version = "0.9.0";
              sha256 = "sha256-rSnLvntgRgMI/8SXLCK2BfambJ0PwygrhFjxSRU4DAw=";
            }
            {
              name = "python";
              publisher = "ms-python";
              version = "2022.15.12451011";
              sha256 = "sha256-zO7L2we37bbn5i/vVhNoxUgMeY5WaPVS895wK8UbT2Q=";
            }
            {
              name = "rainbow-csv";
              publisher = "mechatroner";
              version = "2.4.0";
              sha256 = "sha256-zQW4Juk5+/51Xq6JsR9K645JRBm8lu6r+sgA6/IwtEU=";
            }
            {
              name = "vscode-sql-formatter";
              publisher = "adpyke";
              version = "1.4.4";
              sha256 = "sha256-g4oqB0zV7jB7PeA/d2e8jKfHh+Ci+us0nK2agy1EBxs=";
            }
            {
              name = "lalrpop-highlight";
              publisher= "mnxn";
              version = "0.0.1";
              sha256 = "sha256-teyL4IGx1rtgpXsRtuBft4xlpJrtktYuCl4HaH3pm3c=";
            }
            {
              name = "metals";
              publisher= "scalameta";
              version = "1.20.7";
              sha256 = "sha256-CBelVkq8yjABwU56oYlAqcAjT/N5aq/7pbLMeuULbhU=";
            }
            {
              name = "scala";
              publisher= "scala-lang";
              version = "0.5.6";
              sha256 = "sha256-eizIPazqEb27aQ+o9nTD1O58zbjkHYHNhGjK0uJgnwA=";
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

    feh.enable = true;

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

    network-manager-applet.enable = true;

    blueman-applet.enable = true;

    dunst.enable = true;
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
