{ config, pkgs, ... }:

{
  imports = [
    ./ace-window.nix
    ./appearance
    ./completion
    ./dired.nix
    ./eshell.nix
    ./git.nix
    ./help.nix
    ./keymaps.nix
    ./mail
    ./markup
    ./programming
    ./project.nix
    ./shell.nix
    ./vterm.nix
  ];
  config = {
    home-manager.users.${config.user} = {
      services.emacs = {
        enable = true;
        defaultEditor = true;
      };
      programs.emacs = {
        enable = true;
        package = pkgs.emacs29-pgtk;
        extraConfig = ''
          (defgroup nrde nil
            "Base customization group for user settings."
            :group 'external
            :prefix 'nrde)
          (setq gc-cons-threshold most-positive-fixnum
                gc-cons-percentage 0.6)
          (add-hook 'emacs-startup-hook
                    (lambda ()
                      (setq undo-limit (* 8 1024 1024)
                            read-process-output-max (* 1024 1024))))
          (advice-add 'x-apply-session-resources :override 'ignore)
          (setq native-comp-jit-compilation nil)
          (setq custom-file
               (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                       "/emacs/custom.el"))
          (load custom-file t)
          (setq backup-directory-alist
                `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                                     "/emacs/backup"))))
          (setq bookmark-default-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/bookmarks"))
          (setq auto-save-list-file-prefix
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/auto-save-list"))
          (save-place-mode 1)
          (setq save-place-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/places"))
          (setq history-length 10000)
          (setq savehist-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                        "/emacs/history"))
          (show-paren-mode 1)
          (subword-mode 1)
          (setq-default indent-tabs-mode nil)
          (setq save-interprogram-paste-before-kill t)
          (setq mouse-yank-at-point t)
          (setq require-final-newline t)
          (repeat-mode 1)
          (setq copyright-names-regexp
                (format "%s <%s>" user-full-name user-mail-address))
          (add-hook 'after-save-hook 'copyright-update)
          (add-hook 'before-save-hook 'delete-trailing-whitespace)

          (define-key global-map (kbd "M-K") 'kill-whole-line)
          (define-key global-map (kbd "M-c") 'capitalize-dwim)
          (define-key global-map (kbd "M-l") 'downcase-dwim)
          (define-key global-map (kbd "M-u") 'upcase-dwim)

          (with-eval-after-load 'mwheel
            (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                                ((control) . 1)))
            (setq mouse-wheel-progressive-speed nil)
            (setq mouse-wheel-follow-mouse t)
            (setq scroll-conservatively 100)
            (setq mouse-autoselect-window nil)
            (setq what-cursor-show-names t)
            (setq focus-follows-mouse t))
        '';
      };
    };
  };
}
