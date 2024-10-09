{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    nur.nixosModules.nur
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    ordenada.nixosModules.ordenada
    (
      {
        lib,
        pkgs,
        config,
        ...
      }:
      {
        networking.hostName = "lyra";
        networking.firewall.enable = false;
        time.timeZone = "Europe/Madrid";
        hardware.firmware = [ pkgs.sof-firmware ];
        hardware.enableRedistributableFirmware = true;
        hardware.graphics.enable = true;
        nixpkgs.overlays = overlays;
        environment.systemPackages = with pkgs; [
          emacs
          git
          git-agecrypt
        ];
        boot = {
          initrd.availableKernelModules = [
            "xhci_pci"
            "thunderbolt"
            "usb_storage"
            "usbhid"
          ];
          kernelModules = [
            "kvm-intel"
            "i2c-dev"
            "ddcci"
            "ddcci_backlight"
            "v4l2loopback"
          ];
          extraModulePackages = with config.boot.kernelPackages; [
            ddcci-driver
            v4l2loopback
          ];
          extraModprobeConfig = ''
            options ddcci dyndbg delay=120
            options ddcci-backlight dyndbg
            options v4l2loopback exclusive_caps=1 max_buffers=2 video_nr=-1
          '';
          loader = {
            efi.canTouchEfiVariables = true;
            efi.efiSysMountPoint = "/boot/efi";
            timeout = 0;
            grub = {
              enable = true;
              efiSupport = true;
              enableCryptodisk = true;
              device = "nodev";
            };
          };
          binfmt.emulatedSystems = [ "aarch64-linux" ];
          initrd.luks.devices = {
            system-root = {
              device = "/dev/disk/by-uuid/0f74821b-da48-4f0c-9f94-f39e646da1bf";
            };
          };
        };
        fileSystems = {
          "/" = {
            device = "/dev/mapper/system-root";
            fsType = "ext4";
            options = [ "noatime" ];
          };
          "/boot/efi" = {
            device = "/dev/nvme0n1p1";
            fsType = "vfat";
          };
        };
        services.udev.extraRules = ''
          ACTION=="add", \
          SUBSYSTEM=="backlight", \
          KERNEL="intel_backlight", \
          MODE="0666", \
          RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
        '';
        services.guix.enable = true;
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "${config.home-manager.users.vega.home.homeDirectory}/.ssh/id_ed25519" ];
          secrets = {
            "hosts/lyra/syncthing/key".owner = "vega";
            "hosts/lyra/syncthing/cert".owner = "vega";
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = with config.home-manager.users.vega; {
          enable = true;
          user = "vega";
          key = config.sops.secrets."hosts/lyra/syncthing/key".path;
          cert = config.sops.secrets."hosts/lyra/syncthing/cert".path;
          dataDir = "${xdg.dataHome}/syncthing";
          configDir = "${xdg.configHome}/syncthing";
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              auriga.id = "TA7YNZV-D5NBYBY-AFB2UU5-S4YNLRA-DCVV5CB-QP333MS-K7MVD4O-NIWUBQB";
              orion.id = "JHG7EZC-D52KXLP-AN45CEX-ADKNSST-J4R3XDF-NDI26JH-JIJYZJ5-AEJHFQO";
              taurus.id = "NLIYTEA-LIL3CAW-N62ZGBA-6DRE2ZM-UNPTBLN-J7JQGBY-RVLYJNT-GD6ZTAG";
            };
            folders = {
              documents = {
                path = "~/documents";
                devices = [ "auriga" ];
              };
              pictures = {
                path = "~/pictures";
                devices = [
                  "auriga"
                  "taurus"
                ];
              };
              notes = {
                path = "~/notes";
                devices = [
                  "auriga"
                  "taurus"
                ];
              };
              videos = {
                path = "~/videos";
                devices = [ "auriga" ];
              };
              work-projects = {
                path = "~/src/work";
                devices = [ "orion" ];
              };
              work-notes = {
                path = "~/notes/work";
                devices = [ "orion" ];
              };
              work-documents = {
                path = "~/documents/work";
                devices = [ "orion" ];
              };
            };
          };
        };
        ordenada = {
          users = {
            vega = { };
          };
          features = {
            userInfo.username = "vega";
            keyboard.layout = {
              name = "us,es";
              options = [
                "grp:shifts_toggle"
                "caps:ctrl_modifier"
                "altwin:prtsc_rwin"
              ];
            };
            theme.enable = true;
            theme.polarity = "dark";
            bash.enable = true;
            home.enable = true;
            fontutils.enable = true;
            clojure.enable = true;
            pipewire.enable = true;
            git = {
              enable = true;
              username = "Miguel Ángel Moreno";
              email = "mail@migalmoreno.com";
              signingKey = "5F23F458";
              signCommits = true;
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
              autoStartTty = "/dev/tty1";
              modifier = "Mod4";
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
            kanshi = {
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
            markdown.enable = true;
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
              modus-themes = {
                enable = true;
                dark = config.ordenada.features.theme.polarity == "dark";
              };
              org.enable = true;
              org-roam = {
                enable = true;
                captureTemplates = ''
                  `(("w" "work" plain "%?"
                     :if-new (file+head "work/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t)
                    ("p" "personal" plain "%?"
                     :if-new (file+head "personal/%<%Y%m%d%H%M%S>-''${slug}.org"
                                        "#+title: ''${title}\n#+filetags: :''${Topic}:\n")
                     :unnarrowed t))
                '';
                dailiesDirectory = "./";
                dailiesCaptureTemplates = ''
                  '(("w" "work" entry
                     "* %?"
                     :if-new (file+head "work/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))
                    ("p" "personal" entry
                     "* %?"
                     :if-new (file+head "personal/daily/%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n")))
                '';
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
            };
            direnv.enable = true;
            compile.enable = true;
            ssh = with pkgs.secrets.hosts; {
              enable = true;
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
            networking.enable = true;
            tailscale.enable = true;
            xdg.enable = true;
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
        system.stateVersion = "24.05";
      }
    )
  ];
}
