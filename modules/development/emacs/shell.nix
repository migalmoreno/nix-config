{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraConfig = ''
(with-eval-after-load 'sh-script
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (setq sh-indent-comment nil)
  (setq sh-first-lines-indent nil))
(add-to-list 'display-buffer-alist
             `(,(rx "*Async Shell Command" (* any) "*")
               (display-buffer-no-window)))
(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist
               '("sh" . "src sh")))
(with-eval-after-load 'ob-core
  (require 'ob-shell))
(with-eval-after-load 'project
  (define-key project-prefix-map "s" 'project-shell)
  (add-to-list 'project-switch-commands
               '(project-shell "Start an inferior shell")))
'';
    };
  };
}
