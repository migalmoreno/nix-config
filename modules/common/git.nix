{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.git = {
      enable = true;
      userName = config.secrets.work.fullname;
      userEmail = config.secrets.work.email;
    };
  };
}
