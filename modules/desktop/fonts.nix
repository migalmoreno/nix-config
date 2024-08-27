{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption;
in {
  options = {
    fonts = {
      monospace = mkOption {
        type = types.attrs;
        default = {
          name = "Iosevka Nerd Font";
          package = (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; });
          size = 11;
        };
      };
      serif = mkOption {
        type = types.attrs;
        default = {
          name = "IBM Plex Sans";
          package = pkgs.ibm-plex;
          size = 11;
        };
      };
      sans = mkOption {
        type = types.attrs;
        default = {
          name = "IBM Plex Sans";
          package = pkgs.ibm-plex;
          size = 11;
        };
      };
      unicode = mkOption {
        type = types.attrs;
        default = {
          name = "Noto Color Emoji";
          package = pkgs.noto-fonts-emoji;
          size = 11;
        };
      };
    };
  };
  config = {
    home-manager.users.${config.user} = {
      fonts.fontconfig = {
        enable = true;
        defaultFonts = {
          sansSerif = [ config.fonts.sans.name ];
          serif = [ config.fonts.serif.name ];
          monospace = [ config.fonts.monospace.name ];
          emoji = [ config.fonts.unicode.name ];
        };
      };
      home.packages = with pkgs; [
        config.fonts.monospace.package
        config.fonts.sans.package
        config.fonts.serif.package
        config.fonts.unicode.package
      ];
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
  };
}
