{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [ xdg-utils ];
    xdg =
      let homeDir = config.home-manager.users.${config.user}.home.homeDirectory;
      in {
        enable = true;
        mime.enable = true;
        mimeApps.enable = true;
        portal = {
          enable = true;
          extraPortals = [
            pkgs.xdg-desktop-portal-gtk
            pkgs.xdg-desktop-portal-wlr
          ];
          config = {
            common.default = [ "gtk" "wlr" ];
          };
        };
        userDirs = {
          enable = true;
          createDirectories = true;
          desktop = null;
          documents = "${homeDir}/documents";
          download = "${homeDir}/downloads";
          music = "${homeDir}/music";
          pictures = "${homeDir}/pictures";
          publicShare = "${homeDir}/public";
          templates = null;
          videos = "${homeDir}/videos";
        };
      };
  };
}