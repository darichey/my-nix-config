{ config, pkgs, ... }:

{
  # =================
  # Misc user config
  # =================
  home.username = "david";
  home.homeDirectory = "/home/david";
  home.sessionVariables = {
    EDITOR = "code";
    NIXOS_OZONE_WL = 1;
  };

  # Copy wallpapers into home dir
  home.file.".wallpapers".source = ../wallpapers;
  home.file.".local/share/icons/Catppuccin-Latte-Dark" = {
    recursive = true;
    source = "${pkgs.catppuccin-cursors.latteDark}/share/icons/catppuccin-latte-dark-cursors";
  };

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      general = {
        gaps_in = 0;
        gaps_out = 0;
      };

      animations = {
        enabled = false;
      };

      misc = {
        disable_hyprland_logo = true;
      };

      monitor =
        [
          "DP-1,3840x2160@160,0x0,1.5"
        ];

      bind =
        [
          "SUPER_SHIFT, Q, exit"
          "SUPER_SHIFT, C, killactive"

          "SUPER, T, exec, alacritty"
          "SUPER, R, exec, rofi -show drun"
          "SUPER, F, togglefloating"
          "SUPER, S, exec, grim -g \"$(slurp -d)\" - | wl-copy"

          "SUPER, 1, workspace, 1"
          "SUPER_SHIFT, 1, movetoworkspacesilent, 1"
          "SUPER, 2, workspace, 2"
          "SUPER_SHIFT, 2, movetoworkspacesilent, 2"
          "SUPER, 3, workspace, 3"
          "SUPER_SHIFT, 3, movetoworkspacesilent, 3"
          "SUPER, 4, workspace, 4"
          "SUPER_SHIFT, 4, movetoworkspacesilent, 4"
          "SUPER, 5, workspace, 5"
          "SUPER_SHIFT, 5, movetoworkspacesilent, 5"
          "SUPER, 6, workspace, 6"
          "SUPER_SHIFT, 6, movetoworkspacesilent, 6"
          "SUPER, 7, workspace, 7"
          "SUPER_SHIFT, 7, movetoworkspacesilent, 7"
          "SUPER, 8, workspace, 8"
          "SUPER_SHIFT, 8, movetoworkspacesilent, 8"
          "SUPER, 9, workspace, 9"
          "SUPER_SHIFT, 9, movetoworkspacesilent, 9"
          "SUPER, 0, workspace, 10"
          "SUPER_SHIFT, 0, movetoworkspacesilent, 10"
        ];

      bindm = [
        "SUPER,mouse:272,movewindow"
        "SUPER,mouse:273,resizewindow"
      ];

      exec-once = [
        "waybar"
        "hyprpaper"
        "[workspace 9 silent] discord"
        "[workspace 9 silent] spotify"
        "swayidle -w timeout 600 \"hyprctl dispatch dpms off\" resume \"hyprctl dispatch dpms on\" 2>&1 > ~/somelog.txt"
        "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1"
      ];

      workspace = [
        "1,monitor:DP-1"
        "2,monitor:DP-1"
        "3,monitor:DP-1"
        "4,monitor:DP-1"
        "5,monitor:DP-1"
        "6,monitor:DP-1"
        "7,monitor:DP-1"
        "8,monitor:DP-1"
        "9,monitor:DP-1"
      ];

      windowrulev2 = [
        "workspace 9 silent,^class:(discord)$"
      ];

      env = [
        "HYPRCURSOR_THEME,Catppuccin-Latte-Dark"
        "HYPRCURSOR_SIZE,36"
        "XCURSOR_SIZE,36"
      ];
    };
  };

  # ===============================================================
  # Always-present user packages
  # Remember, per-project packages should be installed per-project!
  # ===============================================================
  home.packages = with pkgs; [
    google-chrome
    firefox
    discord
    spotify
    obs-studio
    vlc
    comma
    gparted
    vlc
    keybase
    kbfs # needed for git-remote-keybase
    cachix
    zip
    unzip
    file
    neofetch
    usbutils
    gnupg
    pinentry-qt
    pavucontrol
    playerctl
    brightnessctl
    at-spi2-core # https://github.com/NixOS/nixpkgs/issues/16327#issuecomment-303068424
    tree
    virt-manager
    mesa-demos
    hyprpaper
    grim
    slurp
    wl-clipboard
    nerdfonts
    swayidle
    sidequest
    polkit_gnome
    amdgpu_top
    jq
    prismlauncher
    temurin-bin-21
  ];

  xdg = {
    # TODO: Convert alacritty config to a nix expression so it can be set using `programs.alacritty.settings`
    configFile."alacritty/alacritty.toml".source = ./alacritty.toml;

    # TODO: https://github.com/nix-community/home-manager/issues/4632
    configFile."hypr/hyprpaper.conf".source = ./hyprpaper.conf;

    mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = [ "thunar.desktop" ];
        "x-scheme-handler/http" = "google-chrome.desktop";
        "x-scheme-handler/https" = "google-chrome.desktop";
      };
    };
  };

  # ==============
  # Program config
  # ==============
  programs = {
    zsh = {
      enable = true;
      autosuggestion.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;

      oh-my-zsh = {
        enable = true;
        custom = "${./zsh-custom}";
        theme = "panda-lambda";
      };
    };

    alacritty.enable = true;

    rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      plugins = [ pkgs.rofi-calc ];
    };

    vscode = {
      enable = true;
      mutableExtensionsDir = true;
      userSettings = {
        "editor.minimap.enabled" = false; # Disable minimap
        "workbench.editor.revealIfOpen" = true;
        "terminal.integrated.scrollback" = 10000;
        "terminal.integrated.shellIntegration.enabled" = false; # work around https://github.com/microsoft/vscode/issues/158052
        "[javascript][javascriptreact][typescript][typescriptreact][json]" = {
          "editor.formatOnSave" = true;
          "editor.defaultFormatter" = "esbenp.prettier-vscode";
        };
        "[rust]" = {
          "editor.formatOnSave" = true;
        };
        "rust-analyzer.checkOnSave.command" = "clippy";
        "editor.linkedEditing" = true;
      };

      extensions =
        let
          packagedExtensions = with pkgs.vscode-extensions; [
            eamodio.gitlens
            jnoortheen.nix-ide
            rust-lang.rust-analyzer
            tamasfe.even-better-toml
            mkhl.direnv
            bradlc.vscode-tailwindcss
            ms-python.python
            mechatroner.rainbow-csv
            esbenp.prettier-vscode
            vadimcn.vscode-lldb
            scalameta.metals
            scala-lang.scala
            ms-vscode-remote.remote-ssh
          ];

          # For packages that aren't in/we don't want to pull from nixpkgs (probably because they're out of date)
          # Can use `./nixpkgs/pkgs/applications/editors/vscode/extensions/update_installed_exts.sh` (See https://nixos.wiki/wiki/Visual_Studio_Code ("Managing Extensions" section))
          # Or pull info from the vscode marketplace page and use pkgs.lib.fakeHash to get the hash
          unpackagedExtensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "vscode-position";
              publisher = "jtr";
              version = "1.1.2";
              sha256 = "sha256-8FZTC26xtFe+2ObT/2UO/qmYipszexgGTJRZNFy3qu8=";
            }
            {
              name = "pdf";
              publisher = "tomoki1207";
              version = "1.2.2";
              sha256 = "sha256-i3Rlizbw4RtPkiEsodRJEB3AUzoqI95ohyqZ0ksROps=";
            }
            {
              name = "godot-tools";
              publisher = "geequlim";
              version = "2.0.0";
              sha256 = "sha256-6lSpx6GooZm6SfUOjooP8mHchu8w38an8Bc2tjYaVfw=";
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

    eza.enable = true;

    htop.enable = true;

    waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          output = "DP-1";
          height = 30;
          spacing = 4;

          modules-left = ["hyprland/workspaces"];
          modules-center = ["hyprland/window"];
          modules-right = ["tray" "idle_inhibitor" "cpu" "memory" "custom/gpu" "pulseaudio" "network" "clock"];

          "hyprland/workspaces" = {
            persistent-workspaces = {
              "DP-1" = [ 1 2 3 4 5 6 7 8 9 ];
            };
          };

          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
                activated = "";
                deactivated = "";
            };
            tooltip-format-activated = "Idle Inhibitor: On";
            tooltip-format-deactivated = "Idle Inhibitor: Off";
          };

          clock = {
            format = "{:%b%e %I:%M%p}";
          };

          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-disconnected = "Disconnected ⚠";
          };

          tray = {
            spacing = 10;
          };

          pulseaudio = {
            format = "{volume}% {icon}";
            format-icons = {
              default = ["" "" ""];
            };
            on-click = "pavucontrol";
          };

          cpu = {
            interval = 10;
            format = "CPU {}%";
            max-length = 10;
          };

          memory = {
            interval = 10;
            format = "RAM {}%";
            max-length = 10;
          };

          "custom/gpu" = {
            exec = "amdgpu_top -d -J | jq '.[0].gpu_activity.GFX.value'";
            format = "GPU {}%";
            interval = 10;
          };
        };
      };
      style = ./waybar.css;
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  # ==============
  # Service config
  # ==============
  services = {
    gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-qt;
    };

    playerctld.enable = true;

    network-manager-applet.enable = true;

    blueman-applet.enable = true;

    dunst.enable = true;
  };

  fonts.fontconfig.enable = true;

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
