{ config, lib, pkgs, ... }:

{
  imports = [
    ./javascript.nix
    ./nix.nix
    ./yaml.nix
  ];
}
