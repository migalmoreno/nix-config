{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption;
in {
  options = {
    fonts = {
      monospace = mkOption {
        type = types.attrs;
        default = {
          name = "Iosevka";
          package = pkgs.iosevka;
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
        dejavu_fonts
        unifont
      ];
    };
  };
}
