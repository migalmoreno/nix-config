{
  config,
  lib,
  pkgs,
  ...
}:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      gnumake
      direnv
    ];
    programs.emacs = lib.mkIf config.home-manager.users.${config.user}.programs.emacs.enable {
      extraPackages = epkgs: with epkgs; [ envrc ];
      extraConfig = ''
        (defun nrde-compile-ansi-color-apply ()
          "Translate control sequences into text properties in compile buffer."
          (interactive)
          (ansi-color-apply-on-region (point-min) (point-max)))

        (add-hook 'compilation-filter-hook 'nrde-compile-ansi-color-apply)

        (eval-when-compile (require 'envrc))
        (add-hook 'after-init-hook 'envrc-global-mode)
        (with-eval-after-load 'envrc
          (define-key envrc-mode-map (kbd "C-c E") 'envrc-command-map))
      '';
    };
  };
}
