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
        ./system/configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.david = import ./home/home.nix;
        }
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
