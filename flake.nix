{
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
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    filestash-nix.url = "github:dermetfan/filestash.nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      systems,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      readDirFilenames =
        dir:
        (lib.pipe dir [
          builtins.readDir
          (lib.mapAttrsToList (name: value: { inherit name value; }))
          (builtins.filter (file: lib.hasSuffix ".nix" file.name || file.value == "directory"))
        ]);
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      flake = rec {
        images = {
          taurus = nixosConfigurations.taurus.config.mobile.outputs.android.android-fastboot-images;
        };
        nixosConfigurations =
          let
            secrets = builtins.fromJSON (builtins.readFile ./secrets/secrets.json);
            overlays = [
              (final: prev: { inherit secrets; })
            ];
            pkgs = import nixpkgs { inherit overlays; };
          in
          lib.pipe (readDirFilenames ./hosts) [
            (map (file: rec {
              name = lib.removeSuffix ".nix" file.name;
              value = import (./hosts + "/${if file.value == "directory" then name else "${name}.nix"}") {
                inherit inputs pkgs overlays;
              };
            }))
            builtins.listToAttrs
          ];
        homeConfigurations = lib.pipe nixosConfigurations [
          (lib.filterAttrs (name: value: lib.hasAttrByPath [ "config" "home-manager" ] value))
          builtins.attrNames
          (lib.concatMap (host: (builtins.attrNames nixosConfigurations.${host}.config.home-manager.users)))
          (map (user: {
            name = user;
            value = user.home;
          }))
          builtins.listToAttrs
        ];
      };
      perSystem =
        { pkgs, ... }:
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              git-agecrypt
              ssh-to-age
              home-manager
              sops
              nixos-rebuild
              nixos-install-tools
            ];
          };
        };
    };
}
