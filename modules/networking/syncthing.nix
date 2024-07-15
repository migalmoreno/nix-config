{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services.syncthing = {
      enable = true;
    };
  };
}
