{
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/DeterminateSystems/nixpkgs-weekly/0.1";

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

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mattpocock-skills = {
      url = "github:mattpocock/skills";
      flake = false;
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      agenix,
      secrets,
      llm-agents,
      mattpocock-skills,
      ...
    }@inputs:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    {
      nixosConfigurations.pikachu = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit secrets; };
        modules = [
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = { inherit llm-agents mattpocock-skills; };
              users.alex.imports = [
                ./home
                ./home/home-pikachu.nix
              ];
            };
          }
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager
          ./hosts/pikachu/configuration.nix
        ];
      };

      # Build an SD card image with `nix build '.#nixosConfigurations.pibox.config.system.build.sdImage'`
      nixosConfigurations.pibox = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit secrets; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./hosts/pibox/configuration.nix
          agenix.nixosModules.default
        ];
      };

      homeConfigurations."alex@glaceon" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-darwin";
        extraSpecialArgs = { inherit llm-agents mattpocock-skills; };
        modules = [
          ./home
          ./home/home-glaceon.nix
        ];
      };

      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
