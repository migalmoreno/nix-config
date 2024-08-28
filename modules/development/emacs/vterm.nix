{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ vterm ];
    };
  };
}
