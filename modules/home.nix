{ config, lib, ... }:

let
  inherit (lib) types mkOption;
in
{
  options = {
    user = mkOption {
      type = types.str;
      description = "Primary user of the system";
    };
  };
  config = {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupFileExtension = "backup";
    home-manager.users.${config.user} = {
      programs.home-manager.enable = true;
      targets.genericLinux.enable = true;
      home.stateVersion = "23.05";
    };
  };
}
