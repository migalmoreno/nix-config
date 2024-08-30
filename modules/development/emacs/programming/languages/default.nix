{ config, lib, pkgs, ... }:

{
  imports = [
    ./clojure.nix
    ./javascript.nix
    ./nix.nix
    ./yaml.nix
  ];
}
