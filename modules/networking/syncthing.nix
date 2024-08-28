{ config, lib, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
  };
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
}
