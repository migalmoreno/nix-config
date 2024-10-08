{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nixos-wsl.nixosModules.default
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    ordenada.nixosModules.ordenada
    sops-nix.nixosModules.sops
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        ordenada.users = {
          saiph = { };
        };
        ordenada.features = {
          userInfo = {
            username = "saiph";
          };
          theme.enable = true;
          theme.polarity = "dark";
          home.enable = true;
          bash.enable = true;
          fontutils.enable = true;
          clojure.enable = true;
          pipewire.enable = true;
          git = with pkgs.secrets.work; {
            enable = true;
            username = fullname;
            email = email;
            gitLinkRemotes = {
              "git.${builtins.elemAt domains 0}" = "git-link-gitlab";
              "git-ext.${builtins.elemAt domains 1}" = "git-link-gitlab";
            };
          };
          gtk = {
            enable = true;
            cursorTheme = {
              name = "Bibata-Modern-${
                if config.ordenada.features.theme.polarity == "dark" then "Classic" else "Ice"
              }";
              package = pkgs.bibata-cursors;
            };
          };
          docker.enable = true;
          javascript.enable = true;
          nix.enable = true;
          yaml.enable = true;
          gnupg = {
            enable = true;
            sshKeys = [ "D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6" ];
          };
          password-store.enable = true;
          sway = rec {
            enable = true;
            autoStartTty = "/dev/pts/0";
            modifier = "Mod2";
            keybindings = {
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
          markdown.enable = true;
          emacs = {
            enable = true;
            advancedUser = true;
            ace-window.enable = true;
            all-the-icons.enable = true;
            appearance.enable = true;
            modus-themes = {
              enable = true;
              dark = config.ordenada.features.theme.polarity == "dark";
            };
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
              strictHooks = [ ];
            };
            tramp.enable = true;
          };
          firefox = {
            enable = true;
            extensions = with config.nur.repos.rycee.firefox-addons; [
              ublock-origin
              multi-account-containers
              tridactyl
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
          xdg = {
            enable = true;
            userDirs = homeDirectory: {
              enable = true;
              createDirectories = true;
              desktop = null;
              documents = "${homeDirectory}/documents";
              download = "${homeDirectory}/downloads";
              music = "${homeDirectory}/music";
              pictures = "${homeDirectory}/pictures";
              publicShare = "${homeDirectory}/public";
              templates = null;
              videos = "${homeDirectory}/videos";
            };
          };
        };
        environment.systemPackages = with pkgs; [
          libreoffice
          git-agecrypt
          pavucontrol
          alacritty
        ];
        networking.hostName = "orion";
        networking.firewall.enable = false;
        security.pki.certificateFiles = [ ../secrets/ca-bundle.crt ];
        systemd.services.wsl-vpnkit = {
          enable = true;
          description = "wsl-vpnkit";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
            Restart = "on-failure";
            KillMode = "mixed";
          };
        };
        system.stateVersion = "22.05";
        users.extraGroups.docker.members = [ "saiph" ];
        time.timeZone = "Europe/Madrid";
        hardware.graphics.enable = true;
        virtualisation.docker = {
          daemon.settings = {
            bip = "10.2.20.10/16";
            dns = pkgs.secrets.work.dnsAddresses;
            dns-search = [ (builtins.elemAt pkgs.secrets.work.domains 0) ];
            live-restore = false;
            default-address-pools = [
              {
                base = "10.1.0.0/16";
                size = 24;
              }
            ];
          };
        };
        virtualisation.oci-containers.containers = {
          whoogle-search = {
            image = "benbusby/whoogle-search";
            ports = [ "5000:5000" ];
            extraOptions = [ "--network=host" ];
            environment = {
              WHOOGLE_MINIMAL = "1";
              WHOOGLE_CONFIG_VIEW_IMAGE = "1";
              WHOOGLE_RESULTS_PER_PAGE = "50";
              WHOOGLE_CONFIG_SEARCH_LANGUAGE = "lang_en";
            };
          };
        };
        wsl = {
          enable = true;
          defaultUser = "saiph";
          wslConf = {
            network.generateResolvConf = true;
            network.generateHosts = true;
            user.default = "saiph";
          };
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "${config.home-manager.users.saiph.home.homeDirectory}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/orion/syncthing/key".owner = "saiph";
            "hosts/orion/syncthing/cert".owner = "saiph";
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = with config.home-manager.users.saiph; {
          enable = true;
          user = "saiph";
          key = config.sops.secrets."hosts/orion/syncthing/key".path;
          cert = config.sops.secrets."hosts/orion/syncthing/cert".path;
          dataDir = "${xdg.dataHome}/syncthing";
          configDir = "${xdg.configHome}/syncthing";
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
            };
            folders = {
              work-projects = {
                path = "~/src/work";
                devices = [ "lyra" ];
              };
              work-notes = {
                path = "~/notes";
                devices = [ "lyra" ];
              };
              work-documents = {
                path = "~/documents";
                devices = [ "lyra" ];
              };
            };
          };
        };
        nixpkgs.overlays = overlays;
        home-manager.users.saiph = {
          systemd.user.services.wsl-clipboard = {
            Unit = {
              Description = "WSL clibpoard sharing";
              After = [ "sway-session.target" ];
            };
            Install = {
              WantedBy = [ "default.target" ];
            };
            Service = {
              Type = "exec";
              ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste --watch /mnt/c/Windows/System32/clip.exe";
              Restart = "on-failure";
            };
          };
        };
      }
    )
  ];
}
