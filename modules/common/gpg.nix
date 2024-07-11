{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };
    programs = {
      gpg = {
        enable = true;
      };
    };
  };
}
