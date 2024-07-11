{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption;
in {
  imports = [
    ./bash.nix
    ./fonts.nix
    ./git.nix
    ./gpg.nix
    ./gtk.nix
    ./terminals.nix
    ./xdg.nix
  ];

  options = {
    user = mkOption {
      type = types.str;
      description = "Primary user of the system";
    };
    secrets = mkOption {
      type = types.attrs;
      default = builtins.fromJSON (builtins.readFile ../../secrets/secrets.json);
    };
  };

  config = {
    nix = {
      extraOptions = ''
experimental-features = nix-command flakes
warn-dirty = false
'';
    };
    hardware.graphics.enable = true;
    nixpkgs.config.allowUnfree = true;
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.${config.user} = {
      programs.home-manager.enable = true;
      targets.genericLinux.enable = true;
      home.stateVersion = "23.05";
    };
  };
}
