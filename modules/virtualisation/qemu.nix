{ config, lib, pkgs, ... }:

{
  services.spice-vdagentd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  home-manager.users.${config.user} = {
    home.packages = with pkgs; [
      qemu
      quickemu
    ];
  };
}
