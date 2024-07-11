{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        primary = {
          layer = "top";
          position = "top";
          height = 30;
          modules-left = ["sway/workspaces" "sway/mode"];
          modules-center = ["sway/window"];
          modules-right = [
            #"pulseaudio"
            "sway/language"
            "battery"
            "clock"
          ];
          "sway/window" = {
            max-length = 50;
          };
          "sway/workspaces" = {
            disable-scroll = true;
            all-outputs = true;
            persistent-workspaces = {
              "1" = [];
              "2" = [];
              "3" = [];
              "4" = [];
              "5" = [];
            };
          };
        };
      };
      style = ''
* {
  font-family: ${config.fonts.monospace.name};
  font-size: 14px;
  min-height: 0;
}

#waybar {
  color: #ffffff;
  background: #1e1e1e;
  border: none;
}

.modules-left {
  margin-left: 0.2em;
}

.modules-right {
  margin-right: 0.5em;
}

#workspaces button {
  background: none;
  border: none;
  border-radius: 0;
  margin: 0.4em;
  padding: 0.2em;
}

#battery, #clock {
  margin-left: 1em;
}
'';
    };
  };
}
