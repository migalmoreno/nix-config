{
  config,
  lib,
  pkgs,
  ...
}:

let
  isWsl = builtins.hasAttr "wsl" config && config.wsl.enable;
in
{
  config = {
    security.polkit.enable = true;
    environment.loginShellInit = ''
      [[ $(tty) == ${if isWsl then "/dev/pts/0" else "/dev/tty1"} ]] && exec sway
    '';
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
    security.pam.services.swaylock = { };
    home-manager.users.${config.user} = {
      home.packages = with pkgs; [
        wev
        wl-clipboard
        libxkbcommon
      ];
      programs.swaylock =
        with config.lib.stylix.colors;
        let
          transparent = "00000000";
        in
        {
          enable = true;
          package = pkgs.swaylock-effects;
          settings = {
            clock = true;
            indicator = true;
            indicator-thickness = 7;
            effect-vignette = "0.5:0.5";
            hide-keyboard-layout = true;
            image = config.stylix.image;
            color = base00;
            inside-color = base00;
            inside-clear-color = base00;
            inside-caps-lock-color = base00;
            inside-ver-color = base00;
            inside-wrong-color = base00;
            key-hl-color = base0B;
            layout-bg-color = base00;
            layout-border-color = base01;
            layout-text-color = base05;
            line-uses-inside = false;
            line-uses-ring = false;
            line-color = transparent;
            line-ver-color = transparent;
            line-clear-color = transparent;
            line-wrong-color = transparent;
            ring-color = base01;
            ring-clear-color = base0A;
            ring-caps-lock-color = base01;
            ring-ver-color = base0B;
            ring-wrong-color = base08;
            separator-color = transparent;
            text-color = base05;
            text-clear-color = base05;
            text-caps-lock-color = base05;
            text-ver-color = base05;
            text-wrong-color = base05;
            font = config.stylix.fonts.sansSerif.name;
          };
        };
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
                position = "0,0";
              }
              {
                criteria = "DP-2";
                mode = "1920x1080";
                position = "1920,0";
              }
            ];
          }
        ];
      };
      programs.bemenu = {
        enable = true;
        settings = with config.lib.stylix.colors.withHashtag; {
          line-height = 34;
          ignorecase = true;
          hp = 10;
          cw = 1;
          ch = 20;
          tf = base05;
          tb = base02;
          ff = base05;
          fb = base01;
          nf = base05;
          nb = base01;
          af = base05;
          ab = base01;
          cf = base05;
          cb = base01;
          hf = base01;
          hb = base0D;
          fn = "${config.fonts.monospace.name} 11";
        };
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
        config = with config.lib.stylix.colors.withHashtag; {
          terminal = "alacritty";
          menu = ''
            ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop \
             --dmenu="${pkgs.bemenu}/bin/bemenu -i -H 34 \
             --fn '${config.fonts.monospace.name} 11' \
             --hp 10 --cw 1 --ch 20 \
             --tf '${base05}' --tb '${base02}' --ff '${base05}' --fb '${base01}' \
             --nf '${base05}' --nb '${base01}' --af '${base05}' --ab '${base01}' \
             --cf '${base05}' --cb '${base01}' --hf '${base01}' --hb '${base0D}'"
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
              bg = "${config.stylix.image} ${config.stylix.imageScalingMode}";
            };
          };
          seat."*" = {
            xcursor_theme = "${config.stylix.cursor.name} ${toString config.stylix.cursor.size}";
          };
          keybindings =
            let
              modifier = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.modifier;
            in
            lib.mkOptionDefault {
              # "${modifier}+j" = "exec swayr next-window current-workspace";
              # "${modifier}+k" = "exec swayr prev-window current-workspace";
              "${modifier}+j" = "focus left";
              "${modifier}+k" = "focus right";
              "${modifier}+Prior" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --increase 5";
              "${modifier}+Next" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --decrease 5";
              "${modifier}+x" = "exec ${pkgs.swaylock-effects}/bin/swaylock";
            };
          floating = {
            titlebar = false;
            border = 2;
            criteria = [ { app_id = "Waydroid"; } ];
          };
          colors =
            with config.lib.stylix.colors;
            with pkgs.lib.nix-rice.color;
            let
              background = base00;
              focused =
                if config.stylix.polarity == "dark" then
                  toRgbHex (darken 50 (hexToRgba withHashtag.base0D))
                else
                  toRgbHex (brighten 50 (hexToRgba withHashtag.base0D));
              indicator = focused;
              unfocused = base01;
              text = base05;
              urgent = base08;
            in
            {
              inherit background;
              urgent = {
                inherit background indicator text;
                border = urgent;
                childBorder = urgent;
              };
              focused = {
                inherit background indicator text;
                border = focused;
                childBorder = focused;
              };
              focusedInactive = {
                inherit background indicator text;
                border = unfocused;
                childBorder = unfocused;
              };
              unfocused = {
                inherit background indicator text;
                border = unfocused;
                childBorder = unfocused;
              };
              placeholder = {
                inherit background indicator text;
                border = unfocused;
                childBorder = unfocused;
              };
            };
          window = {
            titlebar = false;
            border = 2;
          };
          gaps.inner = 12;
          bars = [ ];
        };
      };
    };
  };
}
