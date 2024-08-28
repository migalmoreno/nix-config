{ config, lib, pkgs, ... }:

{
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  home-manager.users.${config.user} = {
    home.packages = [ pkgs.pulseaudio ];
    home.sessionVariables = {
      RTC_USE_PIPEWIRE = "true";
    };
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.pulseaudio-control ];
      extraConfig = ''
(with-eval-after-load 'nrde-keymaps
  (define-key nrde-app-map (kbd "v") 'pulseaudio-control-map))
(with-eval-after-load 'pulseaudio-control
  (define-key pulseaudio-control-map
    'pulseaudio-control-toggle-sink-input-mute-by-index)
  (setq pulseaudio-control-volume-step "5%")
  (setq pulseaudio-control-volume-verbose nil)
  (pulseaudio-control-default-sink-mode)
  (pulseaudio-control-default-source-mode))
'';
    };
  };
}
