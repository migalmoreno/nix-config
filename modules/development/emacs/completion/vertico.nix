{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        vertico
      ];
      extraConfig = ''
(eval-when-compile
  (require 'vertico)
  (require 'vertico-multiform))

(with-eval-after-load 'vertico
  (advice-add
   'vertico--format-candidate :around
   (lambda (orig cand prefix suffix index _start)
     (let ((cand (funcall orig cand prefix suffix index _start)))
       (concat
        (if (= vertico--index index)
            (propertize "» " 'face 'vertico-current)
            "  ")
        cand))))
  (define-key global-map (kbd "s-s") 'vertico-repeat)
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  (setq vertico-cycle t)
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          ;; (file buffer)
          ;; (project-file buffer)
          (info-menu buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-lsp-symbols buffer)
          (consult-xref buffer)
          (embark-keybinding buffer)
          (consult-location buffer)))

  (setq vertico-multiform-commands
        '((telega-chat-with buffer)
          (magit:--author flat)
          ;; For some reason it doesn't have an info-menu
          ;; category and also setting
          ;; marginalia-command-categories doesn't help
          ;; (org-roam-node-find buffer)
          (Info-goto-node buffer)
          (info-lookup-symbol buffer)
          (Info-follow-reference buffer)
          (consult-yank-pop buffer)))

  (autoload 'vertico-multiform-mode "vertico-multiform")
  (vertico-multiform-mode))

(autoload 'vertico-mode "vertico")
(if after-init-time
  (vertico-mode 1)
  (add-hook 'after-init-hook 'vertico-mode))
'';
    };
  };
}