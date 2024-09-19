{
  config,
  lib,
  pkgs,
  ...
}:

let
  toggleTheme = pkgs.writeShellScriptBin "toggle-theme" ''
    current_system=$(readlink /run/current-system)
    specialisation=$(readlink /nix/var/nix/profiles/system/specialisation/light-theme)
    if [ "$current_system" == "$specialisation" ]; then
      sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
      ${pkgs.emacs29-pgtk}/bin/emacsclient -e "(load-theme 'modus-vivendi t)"
      ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme adw-gtk3-dark
    else
      sudo /nix/var/nix/profiles/system/specialisation/light-theme/bin/switch-to-configuration switch
      ${pkgs.emacs29-pgtk}/bin/emacsclient -e "(load-theme 'modus-operandi t)"
      ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme adw-gtk3
    fi
  '';
in
{
  environment.systemPackages = [ toggleTheme ];
  home-manager.users.${config.user} = {
    xdg.desktopEntries.stylix = {
      name = "Toggle Theme";
      exec = "${toggleTheme}/bin/toggle-theme %U";
    };
  };
  security.sudo.extraRules = [
    {
      runAs = "root";
      groups = [ "wheel" ];
      commands = [
        {
          command = "/nix/var/nix/profiles/system/specialisation/light-theme/bin/switch-to-configuration switch";
          options = [ "NOPASSWD" ];
        }
        {
          command = "/nix/var/nix/profiles/system/bin/switch-to-configuration switch";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.nixos-rebuild}/bin/nixos-rebuild switch";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
  stylix.enable = true;
  stylix.autoEnable = false;
  stylix.image = pkgs.fetchurl {
    url = "https://w.wallhaven.cc/full/dg/wallhaven-dgo6pl.jpg";
    sha256 = "09jap8g5232h8ham41jljvm1x7d87wjn0p42dy0x119cqd1ds1i3";
  };
  stylix.polarity = "dark";
  stylix.base16Scheme = {
    base00 = "000000";
    base01 = "1e1e1e";
    base02 = "313131";
    base03 = "303030";
    base04 = "646464";
    base05 = "ffffff";
    base06 = "e0e0e0";
    base07 = "0000c0";
    base08 = "ff5f59";
    base09 = "ff6b55";
    base0A = "d0bc00";
    base0B = "6ae4b9";
    base0C = "00d3d0";
    base0D = "79a8ff";
    base0E = "b6a0ff";
    base0F = "7a6100";
  };
  specialisation.light-theme.configuration = {
    stylix = {
      image = lib.mkForce (
        pkgs.fetchurl {
          url = "https://w.wallhaven.cc/full/28/wallhaven-28vjgm.jpg";
          sha256 = "14b5h86jjimdzfw9krbc90abcd9kgvfhavqqq7xzxjxjbakrkzdl";
        }
      );
      polarity = lib.mkForce "light";
      base16Scheme = lib.mkForce {
        base00 = "ffffff";
        base01 = "f0f0f0";
        base02 = "e0e0e0";
        base03 = "c2c2c2";
        base04 = "c4c4c4";
        base05 = "000000";
        base06 = "595959";
        base07 = "9f9f9f";
        base08 = "a60000";
        base09 = "f5d0a0";
        base0A = "6f5500";
        base0B = "00663f";
        base0C = "005e8b";
        base0D = "3548cf";
        base0E = "e07fff";
        base0F = "624416";
      };
    };
  };
}
