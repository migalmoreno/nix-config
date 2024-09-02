{ config, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.password-store = {
      enable = true;
      package = pkgs.pass-wayland.withExtensions (exts: [ exts.pass-otp ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.home-manager.users.${config.user}.xdg.stateHome}/password-store";
      };
    };
    programs.emacs = {
      extraPackages =
        epkgs: with epkgs; [
          pass
          password-store
          password-store-otp
        ];
      extraConfig = ''
        (eval-when-compile (require 'pass))
        (setq pass-show-keybindings nil)
        (add-hook 'pass-mode-hook 'toggle-truncate-lines)
        (with-eval-after-load 'nrde-keymaps
          (define-key nrde-app-map (kbd "p") 'pass))

        (with-eval-after-load 'auth-source
          (auth-source-pass-enable))

        (autoload 'password-store-dir "password-store")
        (autoload 'password-store-list "password-store")
        (autoload 'consult--read "consult")

        (defun nrde-password-store-consult (arg pass)
          "Interactively search the password store."
          (interactive
           (list current-prefix-arg
                 (consult--read (password-store-list)
                                :prompt "Pass entry: "
                                :sort nil
                                :require-match nil
                                :category 'pass)))
          (funcall (if arg
                       'password-store-url
                       'password-store-copy)
                   pass))

        (define-key global-map (kbd "M-g P") 'nrde-password-store-consult)

        (with-eval-after-load 'password-store
          (defvar nrde-password-store-embark-actions
            (let ((map (make-sparse-keymap)))
              (define-key map "f" 'password-store-copy-field)
              (define-key map "b" 'password-store-url)
              (define-key map "e" 'password-store-edit)
              (define-key map "g" 'password-store-generate)
              (define-key map "r" 'password-store-rename)
              (define-key map "d" 'password-store-remove)
              (define-key map "i" 'password-store-insert)
              map)
            "Keymap for actions for pass entries."))

        (with-eval-after-load 'embark
          (require 'password-store)
          (add-to-list 'embark-keymap-alist
                       '(pass . nrde-password-store-embark-actions)))
      '';
    };
  };
}
