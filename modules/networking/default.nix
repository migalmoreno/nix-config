{ config, ... }:

{
  networking.useDHCP = false;
  networking.networkmanager.enable = true;
  users.users.${config.user} = {
    extraGroups = [ "networkmanager" ];
  };
}
