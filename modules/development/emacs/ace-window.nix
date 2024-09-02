{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ ace-window ];
      extraConfig = ''
        (define-key global-map (kbd "M-o") 'ace-window)
        (with-eval-after-load 'ace-window
          (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
          (setq aw-background nil)
          (setq aw-scope 'frame)
          (setq aw-ignore-current nil)
          (setq aw-display-mode-overlay nil))
        (add-hook 'nix-mode-hook 'electric-pair-local-mode)
      '';
    };
  };
}
