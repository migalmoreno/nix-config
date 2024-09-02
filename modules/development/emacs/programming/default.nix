{ config, ... }:

{
  imports = [ ./languages ];
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages =
          epkgs: with epkgs; [
            apheleia
            eglot
            rainbow-delimiters
            tree-sitter
            tree-sitter-langs
            emacs-conflict
          ];
        extraConfig = ''
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
          (apheleia-global-mode 1)
          (with-eval-after-load 'flymake
            (let ((map flymake-mode-map))
              (define-key map (kbd "M-n") 'flymake-goto-next-error)
              (define-key map (kbd "M-p") 'flymake-goto-prev-error)))
        '';
      };
    };
  };
}
