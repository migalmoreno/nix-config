{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        marginalia
      ];
      extraConfig = ''
(eval-when-compile
  (require 'marginalia))

(marginalia-mode 1)
(with-eval-after-load 'marginalia
  (setq marginalia-align 'left))
'';
    };
  };
}
