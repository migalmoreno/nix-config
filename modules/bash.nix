{ config, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.bash = {
      enable = true;
    };
    programs.emacs.extraConfig = ''
      (with-eval-after-load 'shell
        (setq explicit-shell-file-name "${pkgs.bashInteractive}/bin/bash"))
    '';
  };
}
