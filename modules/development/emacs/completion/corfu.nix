{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        cape
        corfu
        corfu-candidate-overlay
      ];
      extraConfig = ''
(autoload 'corfu-history-mode "corfu-history")
(corfu-history-mode)
(with-eval-after-load 'corfu
  (let ((map corfu-map))
    (define-key map "\t" 'corfu-next)
    (define-key map (kbd "<tab>") 'corfu-next)
    (define-key map (kbd "<backtab>") 'corfu-previous)
    (define-key map (kbd "S-TAB") 'corfu-previous)
    (define-key map (kbd "M-p") 'corfu-doc-scroll-down)
    (define-key map (kbd "M-n") 'corfu-doc-scroll-up)
    (define-key map (kbd "M-d") 'corfu-doc-toggle))

  (defun nrde-corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply 'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map (kbd "M-m") 'nrde-corfu-move-to-minibuffer)

  (defun nrde-corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal 'completion-at-point
                             (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook 'nrde-corfu-enable-in-minibuffer)
  (require 'kind-icon)
  (setq corfu-auto-prefix 2)
  (setq corfu-min-width 60)
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-doc-auto t)
  (setq global-corfu-modes '((not org-mode) t))
  (global-corfu-mode 1)
  (define-key corfu-map (kbd "M-D") 'corfu-doc-toggle)
  (corfu-candidate-overlay-mode 1)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
  (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch))
'';
    };
  };
}
