{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs.extraConfig = ''
      (with-eval-after-load 'tramp
        (setq tramp-verbose 1)
        (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
    '';
  };
}
