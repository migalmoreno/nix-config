{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        orderless
      ];
      extraConfig = ''
(with-eval-after-load 'minibuffer
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((project-file (styles . (orderless partial-completion basic)))
                                        (file (styles . (orderless partial-completion basic)))))
  (setq enable-recursive-minibuffers t))

(defun nrde-completion-crm-indicator (args)
  "Display a discernible indicator for `completing-read-multiple'."
  (cons (concat "[CRM] " (car args)) (cdr args)))

(advice-add 'completing-read-multiple :filter-args 'nrde-completion-crm-indicator)
'';
    };
  };
}
