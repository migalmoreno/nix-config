{ config, lib, pkgs, ... }:

let inherit (lib) types mkOption;
in {
  options = {
    git = {
      username = mkOption {
        type = types.str;
        description = "Primary Git username";
        default = config.user;
      };
      email = mkOption {
        type = types.str;
        description = "Primary Git email";
        default = "";
      };
    };
  };
  config = {
    home-manager.users.${config.user} = {
      programs.git = {
        enable = true;
        userName = config.git.username;
        userEmail = config.git.email;
        signing = {
          signByDefault = !(builtins.hasAttr "wsl" config && config.wsl.enable);
          key = "5F23F458";
        };
      };
    };
  };
}
