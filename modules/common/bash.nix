{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.bash = {
      enable = true;
    };
  };
}
