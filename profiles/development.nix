{ config, pkgs, ... }:

{
  ordenada.features = {
    userInfo = {
      fullName = "Miguel Ángel Moreno";
      email = "mail@migalmoreno.com";
    };
    home.enable = true;
    theme.polarity = "dark";
    keyboard.layout = {
      name = "us,es";
      options = [
        "grp:rctrl_rshift_toggle"
        "caps:ctrl_modifier"
        "altwin:prtsc_rwin"
      ];
    };
    fontutils.enable = true;
    bash.enable = true;
    clojure.enable = true;
    pipewire.enable = true;
    bluetooth.enable = true;
    gtk.enable = true;
    git.enable = true;
    docker.enable = true;
    android.enable = true;
    javascript.enable = true;
    nix.enable = true;
    yaml.enable = true;
    emacs = {
      enable = true;
      advancedUser = true;
      extraConfig = ''
        (setq-default tab-width 2)
        (winner-mode)
        (keymap-set ctl-x-map "C-b" #'ibuffer)
        (with-eval-after-load 'ibuffer
          (setopt ibuffer-expert t))
        (with-eval-after-load 'image-mode
          (keymap-set image-mode-map "q" #'image-kill-buffer)
          (setopt image-use-external-converter t))
        (with-eval-after-load 'mhtml-mode
          (keymap-set html-mode-map "M-o" nil))
        (with-eval-after-load 'ange-ftp
          (setopt ange-ftp-try-passive-mode t))
        (add-hook 'after-save-hook #'delete-trailing-whitespace)
        (with-eval-after-load 'nginx-mode
          (setopt nginx-indent-level 2))
      '';
      extraPackages = with pkgs.emacsPackages; [
        wgrep
        emacs-conflict
        org-present
        nginx-mode
        hcl-mode
      ];
      ace-window.enable = true;
      all-the-icons.enable = true;
      appearance.enable = true;
      modus-themes.enable = true;
      org = {
        enable = true;
        orgModern = true;
      };
      org-roam = {
        enable = true;
        todoIntegration = true;
      };
      pdf-tools.enable = true;
      cider = {
        popReplOnConnect = false;
        replInCurrentWindow = true;
      };
      spelling = {
        enable = true;
        package = pkgs.aspellWithDicts (
          dicts: with dicts; [
            en
            es
          ]
        );
        ispellStandardDictionary = "en_US";
        flyspellHooks = [
          "org-mode-hook"
          "message-mode-hook"
          "bibtex-mode-hook"
        ];
      };
      dired = {
        enable = true;
        killOnNewBuffer = true;
      };
      embark.enable = true;
      corfu = {
        enable = true;
        globalModes = [
          "(not org-mode)"
          "t"
        ];
      };
      consult.enable = true;
      vertico.enable = true;
      completion.enable = true;
      marginalia.enable = true;
      orderless.enable = true;
      ebdb.enable = true;
      eshell.enable = true;
      help.enable = true;
      daemons.enable = true;
      which-key.enable = true;
      time.enable = true;
      calendar = {
        enable = true;
        weekNumbers = true;
      };
      erc = {
        autoQuery = "bury";
        queryDisplay = "buffer";
        joinBuffer = "buffer";
        autojoin = false;
      };
      apheleia.enable = true;
      flymake.enable = true;
      rainbow-delimiters.enable = true;
      eglot.enable = true;
      project.enable = true;
      shell.enable = true;
      vterm.enable = true;
      smartparens = {
        enable = true;
        pareditBindings = true;
        hooks = [
          "prog-mode-hook"
          "lisp-data-mode-hook"
          "minibuffer-inactive-mode-hook"
          "comint-mode-hook"
          "cider-repl-mode-hook"
        ];
        strictHooks = [
          "clojure-mode-hook"
          "scheme-mode-hook"
        ];
      };
      tramp.enable = true;
    };
    sway = {
      enable = true;
      keybindings =
        let
          pamixer = "${pkgs.pamixer}/bin/pamixer";
          playerctl = "${pkgs.playerctl}/bin/playerctl";
        in
        with config.ordenada.features.sway;
        {
          "${modifier}+j" = "focus left";
          "${modifier}+k" = "focus right";
          "XF86AudioMute" = "exec ${pamixer} -t";
          "XF86AudioRaiseVolume" = "exec ${pamixer} --unmute --increase 5";
          "XF86AudioLowerVolume" = "exec ${pamixer} --unmute --decrease 5";
          "XF86AudioPlay" = "exec ${playerctl} play-pause";
          "XF86AudioPause" = "exec ${playerctl} play-pause";
          "XF86AudioNext" = "exec ${playerctl} next";
          "XF86AudioPrev" = "exec ${playerctl} previous";
          "${modifier}+x" = "exec ${pkgs.swaylock-effects}/bin/swaylock";
          "${modifier}+n" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
          "${modifier}+p" = "exec ${pkgs.wlogout}/bin/wlogout";
          "${modifier}+Insert" = "exec pkill -SIGINT -f wf-recorder";
        };
    };
    waybar.enable = true;
    bemenu.enable = true;
    playerctl.enable = true;
    firefox = {
      enable = true;
      extraSettings = {
        "accessibility.typeaheadfind.enablesound" = false;
        "browser.ctrlTab.sortByRecentlyUsed" = true;
        "browser.sessionstore" = false;
        "browser.sessionstore.max_tabs_undo" = 0;
        "browser.sessionstore.resume_session_once" = true;
        "browser.toolbars.bookmarks.visibility" = "never";
        "browser.topsites.contile.enabled" = false;
        "browser.translations.enable" = false;
        "general.smoothScroll" = false;
        "general.autoScroll" = true;
        "identity.fxaccounts.enabled" = false;
        "network.protocol-handler.external.mailto" = false;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
      };
      extraAddons = with pkgs.nur.repos.rycee.firefox-addons; [ react-devtools ];
      extraSearchConfig = {
        order = [
          "Whoogle"
          "Google"
        ];
        engines = {
          "Google".metaData.alias = "@g";
          "Whoogle" = {
            iconUpdateURL = "http://localhost:5000/favicon.ico";
            updateInterval = 24 * 60 * 60 * 1000;
            definedAliases = [ "@w" ];
            urls = [
              {
                template = "http://localhost:5000/search";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
          };
        };
      };
    };
    irc = {
      enable = true;
      accounts = {
        soju = {
          nick = "migalmoreno";
          port = 6667;
          tls = false;
          bouncer = true;
          client = config.networking.hostName;
        };
        libera = {
          network = "irc.libera.chat";
          nick = "migalmoreno";
        };
        oftc = {
          network = "irc.oftc.net";
          nick = "migalmoreno";
        };
      };
    };
    markdown.enable = true;
    direnv.enable = true;
    compile.enable = true;
    ssh = with pkgs.secrets.hosts; {
      enable = true;
      daemon = false;
      matchBlocks = {
        auriga = {
          hostname = auriga.address;
          user = "root";
        };
        capella = {
          hostname = auriga.address;
          user = "capella";
        };
        cygnus = {
          hostname = cygnus.address;
          user = "root";
        };
      };
    };
    xdg.enable = true;
  };
}
