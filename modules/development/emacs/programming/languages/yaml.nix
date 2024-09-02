{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ yaml-mode ];
      extraConfig = ''
        (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
        (with-eval-after-load 'yaml-mode
          (define-key yaml-mode-map (kbd "RET") 'newline-and-indent))
      '';
    };
  };
}
