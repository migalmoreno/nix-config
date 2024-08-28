{ config, lib, pkgs, ... }:

{
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.bluetooth ];
      extraConfig = ''
(with-eval-after-load 'nrde-keymaps
  (define-key nrde-app-map (kbd "B") 'bluetooth-list-devices))
(with-eval-after-load 'bluetooth
  (define-key bluetooth-mode-map "C" 'bluetooth-connect-profile))
'';
    };
  };
}
