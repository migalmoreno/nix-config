{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs
    ./git.nix
  ];
}
