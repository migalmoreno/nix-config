{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-bemenu;
      sshKeys = [ "D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6" ];
    };
    programs = {
      gpg = {
        enable = true;
        homedir = "${config.home-manager.users.${config.user}.xdg.dataHome}/gnupg";
      };
    };
  };
}
