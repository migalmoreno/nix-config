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
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    nixarr.url = "github:rasmus-kirk/nixarr";
    mobile-nixos = {
      url = "github:mobile-nixos/mobile-nixos";
      flake = false;
    };
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs@{ nixpkgs, systems, ... }: let
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f (import nixpkgs { inherit system; }));
  in rec {
    images = {
      taurus = nixosConfigurations.taurus.config.mobile.outputs.android.android-fastboot-images;
    };
    devShells = eachSystem (pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          git-agecrypt
          ssh-to-age
          home-manager
          sops
          nixos-rebuild
          nixos-install-tools
        ];
      };
    });
    nixosConfigurations = {
      auriga = import ./hosts/auriga.nix { inherit inputs; };
      lyra = import ./hosts/lyra.nix { inherit inputs; };
      orion = import ./hosts/orion.nix { inherit inputs; };
      taurus = import ./hosts/taurus.nix { inherit inputs; };
    };
    homeConfigurations = with nixosConfigurations.nixos.config.home-manager.users; {
      capella = capella.home;
      maia = maia.home;
      saiph = saiph.home;
      vega = vega.home;
    };
  };
}
