{
  description = "David's System Config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, ... }: {
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
        # ({ config, pkgs, ... }: {
        #   nixpkgs.config.packageOverrides = pkgs: rec {
        #     discord = pkgs.discord.overrideAttrs (old: {
        #       src = pkgs.fetchurl {
        #         url = "https://dl.discordapp.net/apps/linux/0.0.18/discord-0.0.18.tar.gz";
        #         sha256 = "sha256-BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
        #       };
        #     });
        #   };
        # })
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
