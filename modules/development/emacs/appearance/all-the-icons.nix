{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        all-the-icons
      ];
      extraConfig = ''
(with-eval-after-load 'all-the-icons
  (setq all-the-icons-scale-factor 1.0)
  (setq all-the-icons-default-adjust 0)
  (setq all-the-icons-octicon-scale-factor 0.9))
'';
    };
  };
}
