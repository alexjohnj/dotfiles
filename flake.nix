{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11"; };
  outputs = { self, nixpkgs, ... }@inputs: {
    nixosConfigurations.pikachu = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./hosts/pikachu/configuration.nix ];
    };
  };
}
