{ config, lib, pkgs, ... }:

{
  nix = {
    extraOptions = ''
experimental-features = nix-command flakes
warn-dirty = false
'';
  };
  nixpkgs.config.allowUnfree = true;
}
