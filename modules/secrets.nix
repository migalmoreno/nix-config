{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption;
in {
  options = {
    secrets = mkOption {
      type = types.attrs;
      default = builtins.fromJSON (builtins.readFile ../secrets/secrets.json);
    };
  };
}
