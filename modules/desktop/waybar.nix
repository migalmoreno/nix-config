{ config, lib, ... }:

let
  cfg = config.ordenada.features.waybar;
  findIdx = name: lib.lists.findFirstIndex (m: m == name) null cfg.modules;
in
{
  options = {
    ordenada.features.waybar = {
      enable = lib.mkEnableOption "Waybar feature";
      modules = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "sway/workspaces"
          "sway/window"
          "battery"
          "pulseaudio"
          "sway/language"
          "clock"
        ];
        description = "The list of modules to add to Waybar";
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home-manager.users.${config.user} = {
        programs.waybar = {
          enable = true;
          systemd.enable = true;
          settings.primary = {
            layer = "top";
            position = "top";
            height = 30;
          };
          style = with config.lib.stylix.colors.withHashtag; ''
            * {
              font-family: ${config.stylix.fonts.monospace.name}, FontAwesome;
              font-size: 14px;
              box-shadow: none;
              text-shadow: none;
              min-height: 0;
              margin: 0;
              padding: 0;
            }

            tooltip {
              opacity: 1;
              background: ${base01};
              border: 1px solid ${base02};
            }

            tooltip label {
              color: ${base05};
              padding: 0;
            }

            #waybar {
              color: ${base05};
              background: ${base01};
              border: none;
              margin: 0;
              padding: 0;
            }

            .modules-right label {
              margin: 0.3em 0.2em;
              padding: 0.3em 0.6em;
              background: ${base02};
              border-radius: 0.2em;
            }

            .modules-left {
              margin-left: 0.2em;
            }

            .modules-right {
              margin-right: 0.2em;
            }
          '';
        };
      };
    })
    (lib.mkIf (findIdx "battery" != null) {
      home-manager.users.${config.user}.programs.waybar.settings.primary = {
        modules-right = lib.mkOrder (findIdx "battery" + 1 * 100) [ "battery" ];
        "battery" = {
          format = "{capacity}% {icon}";
          states = {
            empty = 10;
            low = 20;
            half = 50;
            high = 80;
            full = 100;
          };
          format-icons = {
            empty = "";
            low = "";
            half = "";
            high = "";
            full = "";
          };
        };
      };
    })
    (lib.mkIf (findIdx "sway/language" != null) {
      home-manager.users.${config.user}.programs.waybar.settings.primary = {
        modules-right = lib.mkOrder (findIdx "sway/language" + 1 * 100) [ "sway/language" ];
        "sway/language" = {
          format = "{short}";
          on-click = "swaymsg input type:keyboard xkb_switch_layout next";
        };
      };
    })
    (lib.mkIf (findIdx "clock" != null) {
      home-manager.users.${config.user}.programs.waybar.settings.primary = {
        modules-right = lib.mkOrder (findIdx "clock" + 1 * 100) [ "clock" ];
        "clock" = with config.lib.stylix.colors.withHashtag; {
          format = "{:%a %d %b %H:%M}";
          format-alt = "{:%a %d %b (w.%V) %H:%M}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode-mon-col = 3;
            weeks-pos = "right";
            format = {
              weeks = "<span color='${base04}'><b>W{}</b></span>";
              today = "<span color='${base0D}'><b>{}</b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };
      };
    })
    (lib.mkIf (findIdx "sway/window" != null) {
      home-manager.users.${config.user}.programs.waybar.settings.primary = {
        modules-center = lib.mkOrder (findIdx "sway/window" + 1 * 100) [ "sway/window" ];
        "sway/window" = {
          max-length = 50;
        };
      };
    })
    (lib.mkIf (findIdx "pulseaudio" != null) {
      home-manager.users.${config.user}.programs.waybar.settings.primary = {
        modules-right = lib.mkOrder (findIdx "pulseaudio" + 1 * 100) [ "pulseaudio" ];
        "pulseaudio" = {
          format = "{volume}% {icon}";
          format-muted = "";
          format-icons = {
            default = [
              ""
              ""
              ""
            ];
          };
        };
      };
    })
    (lib.mkIf (findIdx "sway/workspaces" != null) {
      home-manager.users.${config.user}.programs.waybar = {
        settings.primary = {
          modules-left = lib.mkOrder (findIdx "sway/workspaces" + 1 * 100) [
            "sway/workspaces"
            "sway/mode"
          ];
          "sway/workspaces" = {
            disable-scroll = true;
            all-outputs = false;
            persistent-workspaces = {
              "1" = [ ];
              "2" = [ ];
              "3" = [ ];
              "4" = [ ];
              "5" = [ ];
            };
          };
        };
        style = with config.lib.stylix.colors.withHashtag; ''
          #workspaces button {
            background: ${base02};
            color: ${base05};
            font-weight: normal;
            border: none;
            border-radius: 0.2em;
            margin: 0.3em 0.2em;
            padding: 0.3em 0.4em;
          }

          #workspaces button.active {
            background: ${base02};
          }

          #workspaces button.persistent {
            background: none;
          }

          #workspaces button.focused {
            background: ${base0D};
            color: ${base01};
          }

          #workspaces button.urgent {
            background: ${base08};
            color: ${base07};
          }
        '';
      };
    })
  ];
}
