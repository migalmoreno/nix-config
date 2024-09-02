{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.ebdb ];
      extraConfig = ''
        (defvar nrde-ebdb-map nil
          "Map to bind EBDB commands under.")
        (define-prefix-command 'nrde-ebdb-map)
        (with-eval-after-load 'nrde-keymaps
          (define-key nrde-app-map (kbd "b") 'nrde-ebdb-map)
          (let ((map nrde-ebdb-map))
            (define-key map "a" 'ebdb-display-all-records)
            (define-key map "c" 'ebdb-create-record-extended)))

        (with-eval-after-load 'ebdb
          (require 'ebdb-i18n)
          (require 'ebdb-vcard)
          (require 'ebdb-org)
          (require 'ebdb-mua)
          (with-eval-after-load 'ebdb-mua
            (setq ebdb-mua-pop-up nil))
          (require 'ebdb-notmuch)
          (require 'ebdb-message)
          (require 'ebdb-ispell)
          (require 'ebdb-gnus)
          (setq ebdb-sources (list "~/documents/contacts"))
          (setq ebdb-default-country nil)
          (setq ebdb-default-window-size 0.4)
          (setq ebdb-dedicated-window 'ebdb)
          (setq ebdb-mail-avoid-redundancy t)
          (setq ebdb-complete-mail 'capf)
          (setq ebdb-completion-display-record nil)
          (setq ebdb-complete-mail-allow-cycling nil)
          (setq ebdb-save-on-exit t)
          (define-key ebdb-mode-map "q" 'kill-this-buffer))
      '';
    };
  };
}
