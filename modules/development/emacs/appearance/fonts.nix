{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ fontaine ];
      extraConfig = ''
(with-eval-after-load 'fontset
  (set-fontset-font t 'symbol "Unifont" nil 'append)
  (set-fontset-font t 'unicode "Unifont" nil 'append)
  (set-fontset-font "fontset-default" nil (font-spec :name "Unifont")))

(setq use-default-font-for-symbols nil)

(require 'fontaine)
(setq fontaine-presets
      '((t :default-family "${config.fonts.monospace.name}"
          :default-height ${toString (config.fonts.monospace.size * 10 - 5)}
          :fixed-pitch-family "${config.fonts.monospace.name}"
          :fixed-pitch-height 1.0
          :variable-pitch-family "${config.fonts.sans.name}"
          :variable-pitch-height 1.0
          :variable-pitch-weight regular)))

(require 'xdg)
(setq fontaine-latest-state-file
      (expand-file-name "emacs/fontaine-latest.state.eld"
                        (or (xdg-cache-home) "~/.cache")))

(defun nrde-font--set-default-fonts ()
  (fontaine-set-preset t))

(if after-init-time
  (when (display-graphic-p) (nrde-font--set-default-fonts))
  (add-hook 'after-init-hook 'nrde-font--set-default-fonts))

(add-hook 'modus-themes-after-load-theme-hook 'fontaine-apply-current-preset)
'';
    };
  };
}
