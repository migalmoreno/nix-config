{ config, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages =
        epkgs: with epkgs; [
          consult
          consult-eglot
          embark-consult
        ];
      extraConfig = ''
        (eval-when-compile
          (require 'consult))

        (defun nrde-goto-line-relative ()
          "Just a wrapper around `consult-goto-line', which uses
        relative line numbers, when narrowing is active."
          (interactive)
          (let ((consult-line-numbers-widen nil))
            (call-interactively 'consult-goto-line)))

        (define-key narrow-map (kbd "g") 'nrde-goto-line-relative)

        (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
        (define-key global-map (kbd "M-y") 'consult-yank-pop)
        (define-key goto-map (kbd "a") 'consult-org-agenda)
        (define-key goto-map (kbd "h") 'consult-org-heading)
        (define-key ctl-x-map "b" 'consult-buffer)
        (define-key help-map "a" 'consult-apropos)
        (define-key global-map (kbd "C-x C-r") 'consult-recent-file)
        (define-key ctl-x-map (kbd "M-:") 'consult-complex-command)
        (define-key ctl-x-4-map "b" 'consult-buffer-other-window)
        (let ((map goto-map))
          (define-key map (kbd "g") 'consult-goto-line)
          (define-key map (kbd "M-g") 'consult-goto-line)
          (define-key map (kbd "l") 'consult-line)
          (define-key map (kbd "o") 'consult-outline)
          (define-key map (kbd "i") 'consult-imenu)
          (define-key map (kbd "m") 'consult-mark)
          (define-key map (kbd "M") 'consult-global-mark)
          (define-key map (kbd "b") 'consult-bookmark))

        (let ((map search-map))
          (define-key map (kbd "f") 'consult-find)
          (define-key map (kbd "g") 'consult-ripgrep)
          (define-key map (kbd "e") 'consult-isearch-history)
          (define-key map (kbd "l") 'consult-line))

        (autoload 'consult-isearch-history "consult")
        (let ((map isearch-mode-map))
          (define-key map (kbd "M-e") 'consult-isearch-history)
          (define-key map (kbd "M-s e") 'consult-isearch-history)
          (define-key map (kbd "M-s l") 'consult-line))

        (with-eval-after-load 'consult
          (setq consult-narrow-key "C-=")
          (setq consult-widen-key "C--"))

        (with-eval-after-load 'embark
          (require 'embark-consult))

        (autoload 'consult-customize "consult" "" nil 'macro)
        (autoload 'consult--customize-set "consult")
        (with-eval-after-load 'consult
          (require 'embark-consult)
          (setq consult-ripgrep-args
                (replace-regexp-in-string "^rg" "${pkgs.ripgrep}/bin/rg" consult-ripgrep-args))
          (consult-customize consult-buffer :preview-key "M-.")
          (consult-customize consult-history :category 'consult-history)
          (consult-customize consult-line :inherit-input-method t))

        (with-eval-after-load 'xref
          (setq xref-show-xrefs-function 'consult-xref))
      '';
    };
  };
}
