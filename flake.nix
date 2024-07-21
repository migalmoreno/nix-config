{
  description = "Personal Nix configuration";
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
    nixarr.url = "github:rasmus-kirk/nixarr";
  };
  outputs = inputs: rec {
    nixosConfigurations = {
      auriga = import ./hosts/auriga.nix { inherit inputs; };
      orion = import ./hosts/orion.nix { inherit inputs; };
    };
    homeConfigurations = {
      capella = nixosConfigurations.nixos.config.home-manager.users.capella.home;
      saiph = nixosConfigurations.nixos.config.home-manager.users.saiph.home;
    };
  };
}
