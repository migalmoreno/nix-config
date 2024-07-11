{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        embark
      ];
      extraConfig = ''
(define-key global-map (kbd "C-.") 'embark-act)
(define-key global-map (kbd "C->") 'embark-become)
(define-key minibuffer-local-map (kbd "M-g") 'embark-become)
(define-key help-map "b" 'embark-bindings)

(with-eval-after-load 'embark
  (setq embark-indicators '(embark-minimal-indicator))
  (setq embark-prompter 'embark-keymap-prompter)
  (setq prefix-help-command 'embark-prefix-help-command))

(with-eval-after-load 'window
  (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Embark Collect "
                        (or "Live" "Completions") "*")
                   nil
                   (window-parameters (mode-line-format . none)))))
'';
    };
  };
}
