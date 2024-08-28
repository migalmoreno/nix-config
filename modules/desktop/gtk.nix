{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      dconf
      adwaita-icon-theme
      gnome-tweaks
    ];
    gtk = {
      enable = true;
      theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome-themes-extra;
      };
      cursorTheme = {
        name = "Bibata-Modern-Classic";
        package = pkgs.bibata-cursors;
      };
      gtk3 = {
        extraConfig = {
          gtk-application-prefer-dark-theme = true;
        };
      };
    };
  };
}
