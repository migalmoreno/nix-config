{ config, lib, pkgs, ... }:

{
  imports = [
    ./consult.nix
    ./corfu.nix
    ./embark.nix
    ./marginalia.nix
    ./orderless.nix
    ./vertico.nix
  ];
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: with epkgs; [
          all-the-icons-completion
          kind-icon
        ];
        extraConfig = ''
(defgroup nrde-completion nil
  "Tweaks to the built-in Emacs completion."
  :group 'nrde)

(with-eval-after-load 'minibuffer
  (setq tab-always-indent 'complete)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
  (setq completion-show-help nil)
  (setq completions-format 'one-column)
  (setq completions-header-format nil)
  (let ((map minibuffer-mode-map))
    (define-key map (vector 'remap 'next-line) 'minibuffer-next-completion)
    (define-key map (vector 'remap 'previous-line) 'minibuffer-previous-completion))
  (let ((map completion-in-region-mode-map))
    (define-key map (kbd "C-n") 'minibuffer-next-completion)
    (define-key map (kbd "C-p") 'minibuffer-previous-completion))
  (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(autoload 'all-the-icons-completion-mode "all-the-icons-completion")
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)
'';
      };
    };
  };
}
