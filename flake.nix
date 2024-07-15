{
  description = "Personal Nix configurations.";
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/nixos-wsl/main";
    nur.url = "github:nix-community/nur";
    sops-nix.url = "github:mic92/sops-nix";
    agenix.url = "github:ryantm/agenix";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
  };
  outputs = inputs: rec {
    nixosConfigurations = {
      nixos = import ./hosts/nixos.nix { inherit inputs; };
      auriga = import ./hosts/auriga.nix { inherit inputs; };
    };
    homeConfigurations = {
      nixos = nixosConfigurations.nixos.config.home-manager.users.nixos.home;
      capella = nixosConfigurations.nixos.config.home-manager.users.capella.home;
    };
  };
}
