{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    secrets = {
      url = "git+ssh://git@github.com/alexjohnj/nix-secrets.git?shallow=1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.agenix.follows = "agenix";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      nixos-cosmic,
      home-manager,
      agenix,
      secrets,
      ...
    }@inputs:
    {
      nixosConfigurations.pikachu = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit secrets; };
        modules = [
          {
            nix.settings = {
              substituters = [ "https://cosmic.cachix.org/" ];
              trusted-public-keys = [ "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE=" ];
            };

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.alex.imports = [
                ./home
                ./home/home-pikachu.nix
              ];
            };
          }
          nixos-cosmic.nixosModules.default
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager
          ./hosts/pikachu/configuration.nix
        ];
      };

      homeConfigurations."alex@glaceon" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        modules = [ ./home ];
      };
    };
}
