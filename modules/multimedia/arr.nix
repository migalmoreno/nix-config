{ config, lib, pkgs, ... }:

{
  nixarr = {
    enable = true;
    jellyfin.enable = true;
    radarr.enable = true;
    sonarr.enable = true;
    prowlarr.enable = true;
    transmission.enable = true;
  };
}
