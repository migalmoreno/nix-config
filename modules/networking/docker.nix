{ config, lib, pkgs, ... }:

{
  virtualisation.docker.enable = true;
  users.users.${config.user}.extraGroups = [ "docker" ];
}
