{ config, pkgs, ... }:

{
  programs.adb.enable = true;
  users.users.${config.user}.extraGroups = [ "adbusers" ];
  services.udev.packages = [ pkgs.android-udev-rules ];
  virtualisation.waydroid.enable = true;
  environment.systemPackages = [ pkgs.wl-clipboard ];
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      android-tools
      payload-dumper-go
      fdroidcl
    ];
  };
}
