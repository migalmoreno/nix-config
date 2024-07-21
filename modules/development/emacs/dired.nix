{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [ zip unzip rsync ];
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [
        all-the-icons-dired
        dired-rsync
      ];
      extraConfig = ''
(eval-when-compile (require 'dired))
(defun nrde-dired-open-externally ()
  "Open marked files in Dired through an external program."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (mapc 'embark-open-externally files)))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'toggle-truncate-lines)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (with-eval-after-load 'all-the-icons-dired
    (setq all-the-icons-dired-monochrome nil))
  (let ((map dired-mode-map))
    (define-key map "V" 'nrde-dired-open-externally)
    (define-key map (kbd "C-c C-r") 'dired-rsync)
    (define-key map "q" 'kill-current-buffer))
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-l -A --time-style=long-iso --group-directories-first -h")
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq delete-by-moving-to-trash nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-recursive-copies 'always))

(with-eval-after-load 'dired-rsync
  (setq dired-rsync-options
        "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete"))
(with-eval-after-load 'ls-lisp
  (setq ls-lisp-use-insert-directory-program nil))
'';
    };
  };
}
