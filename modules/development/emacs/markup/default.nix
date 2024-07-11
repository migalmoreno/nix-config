{ config, lib, pkgs, ... }:

{
  imports = [
    ./markdown.nix
    ./org.nix
    ./org-roam.nix
    ./spelling.nix
  ];
}
