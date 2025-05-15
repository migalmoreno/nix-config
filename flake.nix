{
  description = "Personal Nix configuration";
  inputs = {
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    ordenada = {
      url = "github:migalmoreno/ordenada";
      inputs = {
        home-manager.follows = "home-manager";
        nur.follows = "nur";
      };
    };
    nix-rice.url = "github:bertof/nix-rice";
    systems.url = "github:nix-systems/default";
    filestash-nix.url = "github:dermetfan/filestash.nix";
  };
  outputs =
    inputs@{ nixpkgs, systems, ... }:
    let
      inherit (nixpkgs) lib;
      eachSystem = f: lib.genAttrs (import systems) (system: f (import nixpkgs { inherit system; }));
      readDirFilenames =
        dir:
        (builtins.filter (path: lib.hasSuffix ".nix" path) (builtins.attrNames (builtins.readDir dir)));
    in
    rec {
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
      nixosConfigurations =
        let
          secrets = builtins.fromJSON (builtins.readFile ./secrets/secrets.json);
          overlays = [
            inputs.nix-rice.overlays.default
            (final: prev: { inherit secrets; })
          ];
          pkgs = import nixpkgs { inherit overlays; };
        in
        builtins.listToAttrs (
          map (path: {
            name = lib.removeSuffix ".nix" path;
            value = import (./hosts + "/${path}") { inherit inputs pkgs overlays; };
          }) (readDirFilenames ./hosts)
        );
      homeConfigurations = builtins.listToAttrs (
        map
          (user: {
            name = user;
            value = user.home;
          })
          (
            lib.concatMap (host: (builtins.attrNames nixosConfigurations.${host}.config.home-manager.users)) (
              builtins.attrNames nixosConfigurations
            )
          )
      );
    };
}
