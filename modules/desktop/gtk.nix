{
  config,
  lib,
  pkgs,
  ...
}:

let
  cursorName =
    if config.stylix.polarity == "dark" then "Bibata-Modern-Classic" else "Bibata-Modern-Ice";
in
{
  stylix.cursor = {
    name = cursorName;
    package = pkgs.bibata-cursors;
    size = 24;
  };
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      dconf
      adwaita-icon-theme
      gnome-tweaks
    ];
    home.pointerCursor = lib.mkIf (config.stylix.enable != true) {
      gtk.enable = true;
      package = pkgs.bibata-cursors;
      name = cursorName;
    };
    gtk = lib.mkIf (config.stylix.targets.gtk.enable != true) {
      enable = true;
      theme = {
        name = if config.stylix.polarity == "dark" then "adw-gtk3-dark" else "adw-gtk3";
        package = lib.mkForce pkgs.adw-gtk3;
      };
      cursorTheme = {
        name = cursorName;
        package = pkgs.bibata-cursors;
      };
    };
  };
}
