{ config, lib, pkgs, ... }:

{
  imports = [
    ./all-the-icons.nix
    ./fonts.nix
    ./modus-themes.nix
  ];
  config = {
    home-manager.users.${config.user} = {
      imports = with config.nur.repos.rycee.hmModules; [ emacs-init ];
      programs.emacs.init = {
        enable = true;
        earlyInit = ''
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push (cons 'left-fringe 8) default-frame-alist)
(push (cons 'right-fringe 8) default-frame-alist)
(push '(no-special-glyphs) default-frame-alist)
(push '(undecorated) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(push '(internal-border-width . 8) default-frame-alist)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
'';
      };
      programs.emacs = {
        extraPackages = epkgs: with epkgs; [ minions ];
        extraConfig = ''
(require 'xdg)
(setq minibuffer-message-timeout 0)
(pixel-scroll-precision-mode 1)
(tooltip-mode 0)
(setq mode-line-compact 'long)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(set-default 'cursor-type '(bar . 1))
(setq-default cursor-in-non-selected-windows nil)
(setq bookmark-set-fringe-mark nil)
(with-eval-after-load 'menu-bar
  (menu-bar-mode 0))
(with-eval-after-load 'tool-bar
  (tool-bar-mode 0))
(with-eval-after-load 'scroll-bar
  (scroll-bar-mode 0))
(with-eval-after-load 'fringe
  (fringe-mode 8))
(set-frame-parameter nil 'internal-border-width 8)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq window-divider-default-right-width 8)
(setq window-divider-default-bottom-width 8)
(window-divider-mode)
(setq frame-inhibit-implied-resize t)
(setq frame-title-format '(multiple-frames "%b" ("" "%b")))
(with-eval-after-load 'minions-autoloads
  (minions-mode))
(with-eval-after-load 'minions
  (setq minions-mode-line-lighter ";"))
(setq minions-mode-line-minor-modes-map
      (let ((map (make-sparse-keymap)))
        (define-key map (vector 'header-line 'down-mouse-1)
                    'minions-minor-modes-menu)
        map))
(defun rde--move-mode-line-to-header ()
  "Move mode-line to header-line.
This function is needed for various modes to set up the mode-line late."
  (setq-local header-line-format mode-line-format)
  (setq-local mode-line-format nil))

(add-hook 'calendar-initial-window-hook 'rde--move-mode-line-to-header)
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

(setcdr (assq 'vc-mode header-line-format)
        '((:eval (truncate-string-to-width vc-mode 25 nil nil "..."))))
'';
      };
    };
  };
}
