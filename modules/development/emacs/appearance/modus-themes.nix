{ config, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ modus-themes ];
      extraConfig = ''
        (eval-when-compile
          (require 'modus-themes)
          (require 'cl-seq))
        (require 'modus-vivendi-theme)
        (eval-when-compile
          (enable-theme 'modus-vivendi))
        (defgroup nrde-modus-themes nil
          "Configuration related to `modus-themes'."
          :group 'nrde)

        (defcustom nrde-modus-themes-mode-line-padding 1
          "The padding of the mode line."
          :type 'number
          :group 'nrde-modus-themes)
        (defcustom nrde-modus-themes-tab-bar-padding 1
          "The padding of the tab bar."
          :type 'number
          :group 'nrde-modus-themes)
        (defcustom nrde-modus-themes-header-line-padding 1
          "The padding of the header line."
          :type 'number
          :group 'nrde-modus-themes)

        (defcustom nrde-modus-themes-after-enable-theme-hook nil
          "Normal hook run after enabling a theme."
          :type 'hook
          :group 'nrde-modus-themes)

        (defun nrde-modus-themes-run-after-enable-theme-hook (&rest _args)
          "Run `nrde-modus-themes-after-enable-theme-hook'."
          (run-hooks 'nrde-modus-themes-after-enable-theme-hook))

        (defun nrde-modus-themes-set-custom-faces (&optional _theme)
          "set faces based on the current theme."
          (interactive)
          (when (modus-themes--current-theme)
            (modus-themes-with-colors
              (custom-set-faces
               `(window-divider ((,c :foreground ,bg-main)))
               `(window-divider-first-pixel ((,c :foreground ,bg-main)))
               `(window-divider-last-pixel ((,c :foreground ,bg-main)))
               `(vertical-border ((,c :foreground ,bg-main)))
               `(tab-bar
                 ((,c :background ,bg-dim
                      :box (:line-width ,nrde-modus-themes-tab-bar-padding
        				:color ,bg-dim))))
               `(mode-line
                 ((,c :box (:line-width ,nrde-modus-themes-mode-line-padding
        				:color ,bg-mode-line-active))))
               `(mode-line-inactive
                 ((,c :box (:line-width ,nrde-modus-themes-mode-line-padding
        				:color ,bg-mode-line-inactive))))
               `(header-line
                 ((,c :box (:line-width ,nrde-modus-themes-header-line-padding
        				:color ,bg-dim))))
               `(git-gutter-fr:added
                 ((,c :foreground ,bg-added-fringe :background ,bg-main)))
               `(git-gutter-fr:deleted
                 ((,c :foreground ,bg-removed-fringe :background ,bg-main)))
               `(git-gutter-fr:modified
                 ((,c :foreground ,bg-changed-fringe :background ,bg-main)))
               `(aw-leading-char-face
                 ((,c :height 1.0 :foreground ,blue-cooler)))))))

        (setq nrde-modus-themes-header-line-padding 4)
        (setq nrde-modus-themes-tab-bar-padding 4)
        (setq nrde-modus-themes-mode-line-padding 4)
        (advice-add 'enable-theme
                    :after 'nrde-modus-themes-run-after-enable-theme-hook)
        (add-hook 'nrde-modus-themes-after-enable-theme-hook 'nrde-modus-themes-set-custom-faces)
        (with-eval-after-load 'nrde-keymaps
          (define-key nrde-toggle-map (kbd "t") 'modus-themes-toggle))
        (with-eval-after-load 'modus-themes
          (setq modus-themes-common-palette-overrides
            '((border-mode-line-active unspecified)
              (border-mode-line-inactive unspecified)
              (fringe unspecified)
              (fg-line-number-inactive "gray50")
              (fg-line-number-active fg-main)
              (bg-line-number-inactive unspecified)
              (bg-line-number-active unspecified)
              (bg-region bg-ochre)
              (fg-region unspecified)))
            (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
            (setq modus-themes-italic-constructs t)
            (setq modus-themes-bold-constructs t)
            (setq modus-themes-mixed-fonts t)
            (setq modus-themes-org-blocks 'gray-background)
            (setq modus-themes-headings (quote ((1 . (1.15))
                                                (2 . (1.1))
                                                (3 . (1.1))
                                                (4 . (1.0))
                                                (5 . (1.0))
                                                (6 . (1.0))
                                                (7 . (0.9))
                                                (8 . (0.9))))))
        (load-theme 'modus-vivendi t)
      '';
    };
  };
}
