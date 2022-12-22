{
  description = "David's System Config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, agenix, ... }: {
    nixosConfigurations.davids-desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./system/desktop
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.david = import ./home/home.nix;
        }
        ({ config, pkgs, ... }: {
          nixpkgs.config.packageOverrides = pkgs: rec {
            discord = pkgs.discord.overrideAttrs (old: {
              src = pkgs.fetchurl {
                url = "https://dl.discordapp.net/apps/linux/0.0.20/discord-0.0.20.tar.gz";
                sha256 = "sha256-3f7yuxigEF3e8qhCetCHKBtV4XUHsx/iYiaCCXjspYw=";
              };
            });
          };
        })
        agenix.nixosModule
      ];
    };

    nixosConfigurations.davids-laptop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./system/laptop
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.david = import ./home/home.nix;
        }
        agenix.nixosModule
      ];
    };

    devShells."x86_64-linux".default =
      let
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
      in
      pkgs.haskellPackages.developPackage {
        root = ./home/xmonad-config;
        returnShellEnv = true;
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install haskell-language-server ]);
      };
  };
}
