{ config, lib, pkgs, ... }:

let isWsl = builtins.hasAttr "wsl" config && config.wsl.enable;
in {
  config = {
    security.polkit.enable = true;
    environment.loginShellInit = ''
[[ $(tty) == ${if isWsl then "/dev/pts/0" else "/dev/tty1"} ]] && exec sway
'';
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
    security.pam.services.swaylock = {};
    home-manager.users.${config.user} = {
      home.packages = with pkgs; [
        bemenu
        j4-dmenu-desktop
        swayr
        wev
        wl-clipboard
        libxkbcommon
        swaylock-effects
        swaybg
      ];
      programs.swayr = {
        enable = true;
        systemd.enable = true;
      };
      services.kanshi = {
        enable = true;
        settings = [
          {
            profile.name = "headless";
            profile.outputs = [
              {
                criteria = "eDP-1";
                status = "enable";
              }
            ];
          }
          {
            profile.name = "single-left";
            profile.outputs = [
              {
                criteria = "eDP-1";
                status = "disable";
              }
              {
                criteria = "HDMI-A-1";
                status = "enable";
              }
            ];
          }
          {
            profile.name = "single-right";
            profile.outputs = [
              {
                criteria = "eDP-1";
                status = "disable";
              }
              {
                criteria = "DP-2";
                status = "enable";
              }
            ];
          }
          {
            profile.name = "multi";
            profile.outputs = [
              {
                criteria = "eDP-1";
                status = "disable";
              }
              {
                criteria = "HDMI-A-1";
                mode = "1920x1080";
                position = "-1920,0";
              }
              {
                criteria = "DP-2";
                mode = "1920x1080";
                position = "0,0";
              }
            ];
          }
        ];
      };
      wayland.windowManager.sway = {
        enable = true;
        systemd.enable = true;
        xwayland = true;
        wrapperFeatures = {
          base = true;
          gtk = true;
        };
        extraSessionCommands = ''
          export QT_QPA_PLATFORM=wayland
          export QT_WAYLAND_DISABLE_WINDOWDECORATIONS=1
          export XDG_SESSION_TYPE=wayland
          export XDG_CURRENT_DESKTOP=sway
          export SDL_VIDEODRIVER=wayland
          export _JAVA_AWT_WM_NONREPARENTING=1
        '';
        config = {
          terminal = "alacritty";
          menu = ''
           ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop \
            --dmenu="${pkgs.bemenu}/bin/bemenu -i -H 30 --fn ${config.fonts.monospace.name}"
            '';
          defaultWorkspace = "workspace number 1";
          modifier = if isWsl then "Mod2" else "Mod4";
          input = {
            "type:keyboard" = {
              xkb_layout = "us,es";
              xkb_options = "grp:shifts_toggle,caps:ctrl_modifier,altwin:prtsc_rwin";
            };
            "type:touchpad" = {
              dwt = "enabled";
              tap = "enabled";
              middle_emulation = "enabled";
            };
          };
          output = {
            "*" = {
              bg = "${(builtins.fetchurl {
                url = "https://w.wallhaven.cc/full/dg/wallhaven-dgo6pl.jpg";
                sha256 = "09jap8g5232h8ham41jljvm1x7d87wjn0p42dy0x119cqd1ds1i3";
              })} fill";
            };
            eDP-1 = {
              scale = "1";
            };
          };
          keybindings =
            let modifier = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.modifier;
            in lib.mkOptionDefault {
              # "${modifier}+j" = "exec swayr next-window current-workspace";
              # "${modifier}+k" = "exec swayr prev-window current-workspace";
              "${modifier}+j" = "focus left";
              "${modifier}+k" = "focus right";
              "${modifier}+Prior" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --increase 5";
              "${modifier}+Next" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --decrease 5";
            };
          floating = {
            titlebar = false;
            border = 2;
            criteria = [
              {
                app_id = "Waydroid";
              }
            ];
          };
          colors = {
            focused = {
              background = "#285577";
              border = "#00BCFF";
              childBorder = "#285577";
              indicator = "#2e9ef4";
              text = "#ffffff";
            };
          };
          window = {
            titlebar = false;
            border = 2;
          };
          gaps.inner = 12;
          bars = [];
        };
      };
    };
  };
}
