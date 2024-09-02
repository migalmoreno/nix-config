{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages =
        epkgs: with epkgs; [
          magit
          forge
          git-link
        ];
      extraConfig =
        let
          primaryWorkDomain = builtins.elemAt config.secrets.work.domains 0;
          secondaryWorkDomain = builtins.elemAt config.secrets.work.domains 1;
        in
        ''
          (add-hook 'magit-mode-hook 'toggle-truncate-lines)
          (with-eval-after-load 'project
            (define-key project-prefix-map "m" 'magit-project-status)
            (add-to-list 'project-switch-commands
                         '(magit-project-status "Show Magit Status")))
          (with-eval-after-load 'magit
            (define-key magit-mode-map "q" 'magit-kill-this-buffer)
            (setq magit-display-buffer-function
                  'magit-display-buffer-same-window-except-diff-v1)
            (setq magit-pull-or-fetch t)
            (require 'forge))
          (with-eval-after-load 'nrde-keymaps
            (define-key nrde-app-map (kbd "g l") 'git-link))
          (with-eval-after-load 'git-link
            (add-to-list 'git-link-remote-alist
                         '("git.${primaryWorkDomain}" git-link-gitlab))
            (add-to-list 'git-link-remote-alist
                         '("git-ext.${secondaryWorkDomain}" git-link-gitlab))
            (add-to-list 'git-link-commit-remote-alist
                         '("git.${primaryWorkDomain}" git-link-gitlab))
            (add-to-list 'git-link-commit-remote-alist
                         '("git-ext.${secondaryWorkDomain}" git-link-gitlab)))
        '';
    };
  };
}
