{ config, lib, pkgs, ... }:

{
  config = {
    environment.loginShellInit = ''
[[ $(tty) == /dev/pts/0 ]] && exec sway
'';
    home-manager.users.${config.user} = {
      home.packages = with pkgs; [
        bemenu
        j4-dmenu-desktop
        swayr
        wev
        wl-clipboard
        libxkbcommon
      ];
      programs.swayr = {
        enable = true;
        systemd.enable = true;
      };
      wayland.windowManager.sway = {
        xwayland = true;
        enable = true;
        wrapperFeatures = {
          base = true;
          gtk = true;
        };
        config = {
          terminal = "alacritty";
          menu = "${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --dmenu='${pkgs.bemenu}/bin/bemenu -i -H 30 --fn ${config.fonts.monospace.name}'";
          defaultWorkspace = "workspace number 1";
          modifier = "Mod2";
          # input = {
          #   "type:keyboard" = {
          #     xkb_layout = "us,es";
          #     xkb_switch_layout = "next";
          #     xkb_options = "grp:ctrl_space_toggle,caps:ctrl_modifier,altwin:prtsc_rwin";
          #   };
          # };
          output = {
            "*" = {
              bg = "#000000 solid_color";
            };
            eDP-1 = {
              scale = "1";
            };
          };
          keybindings =
            let modifier = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.modifier;
            in lib.mkOptionDefault {
              # "${modifier}+j" = "exec swayr next-window current-workspace";
              # "${modifier}+k" = "exec swayr prev-window current-workspace"
              "${modifier}+j" = "focus left";
              "${modifier}+k" = "focus right";
              "${modifier}+Prior" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --increase 5";
              "${modifier}+Next" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --decrease 5";
            };
          floating = {
            titlebar = false;
            border = 2;
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
          gaps = {
            inner = 8;
          };
          bars = [];
        };
      };
    };
  };
}
