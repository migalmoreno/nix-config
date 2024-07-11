{ config, lib, pkgs, ... }:

{
  imports = [
    ./chrome.nix
    ./firefox.nix
  ];
}
