{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.bash = {
      enable = true;
    };
    programs.emacs.extraConfig = lib.mkIf config.home-manager.users.${config.user}.programs.emacs.enable ''
(with-eval-after-load 'shell
  (setq explcit-shell-file-name "${pkgs.bash}/bin/bash"))
'';
  };
}
