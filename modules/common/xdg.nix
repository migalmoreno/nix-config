{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    xdg =
      let homeDir = config.home-manager.users.${config.user}.home.homeDirectory;
      in {
        enable = true;
        mime.enable = true;
        mimeApps.enable = true;
        userDirs = {
          enable = true;
          createDirectories = true;
          desktop = null;
          documents = "${homeDir}/documents";
          download = "${homeDir}/downloads";
          music = "${homeDir}/music";
          pictures = "${homeDir}/pictures";
          publicShare = null;
          templates = null;
          videos = "${homeDir}/videos";
        };
      };
  };
}
