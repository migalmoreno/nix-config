{ config, pkgs, ... }:

{
  ordenada.features = {
    home.enable = true;
    theme.enable = true;
    theme.polarity = "dark";
    keyboard.layout = {
      name = "us,es";
      options = [
        "grp:shifts_toggle"
        "caps:ctrl_modifier"
        "altwin:prtsc_rwin"
      ];
    };
    fontutils.enable = true;
    bash.enable = true;
    clojure = {
      enable = true;
      cider = {
        popReplOnConnect = false;
        replInCurrentWindow = true;
      };
    };
    pipewire.enable = true;
    bluetooth.enable = true;
    gtk.enable = true;
    git.enable = true;
    docker.enable = true;
    javascript.enable = true;
    nix.enable = true;
    yaml.enable = true;
    emacs = {
      enable = true;
      advancedUser = true;
      extraPackages = with pkgs.emacsPackages; [
        wgrep
        emacs-conflict
      ];
      ace-window.enable = true;
      all-the-icons.enable = true;
      appearance.enable = true;
      modus-themes.enable = true;
      org.enable = true;
      org-roam = {
        enable = true;
        todoIntegration = true;
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
      dired.enable = true;
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
        strictHooks = [ "clojure-mode-hook" ];
      };
      tramp.enable = true;
    };
    sway = {
      enable = true;
      keybindings = with config.ordenada.features.sway; {
        "${modifier}+j" = "focus left";
        "${modifier}+k" = "focus right";
        "${modifier}+Prior" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --increase 5";
        "${modifier}+Next" = "exec ${pkgs.pamixer}/bin/pamixer --unmute --decrease 5";
        "${modifier}+x" = "exec ${pkgs.swaylock-effects}/bin/swaylock";
        "${modifier}+Insert" = "exec pkill -SIGINT -f wf-recorder";
      };
    };
    waybar.enable = true;
    bemenu.enable = true;
    firefox = {
      enable = true;
      extensions = with config.nur.repos.rycee.firefox-addons; [
        ublock-origin
        multi-account-containers
      ];
      extraSearchConfig = {
        default = "Whoogle";
        privateDefault = "Whoogle";
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
