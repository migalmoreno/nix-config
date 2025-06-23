{ inputs, overlays, ... }:

inputs.nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        imports = with inputs; [
          nixos-hardware.nixosModules.raspberry-pi-4
          nixarr.nixosModules.default
          sops-nix.nixosModules.sops
          filestash-nix.nixosModules.filestash
          ../profiles/cgit.nix
          ../profiles/nix.nix
          ../profiles/tailscale.nix
        ];
        hardware = {
          enableRedistributableFirmware = true;
          raspberry-pi."4" = {
            apply-overlays-dtmerge.enable = true;
            fkms-3d.enable = true;
          };
        };
        hardware.graphics.enable = true;
        networking = {
          hostName = "auriga";
          useDHCP = false;
          interfaces.wlan0.useDHCP = true;
          firewall = {
            enable = true;
            allowedTCPPorts = [
              6667
              3000
            ];
          };
          networkmanager.enable = true;
        };
        nixpkgs.overlays = overlays;
        boot = {
          kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
          kernelModules = [ "v4l2loopback" ];
          extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
          initrd.availableKernelModules = [
            "xhci_pci"
            "usbhid"
            "usb_storage"
          ];
          loader = {
            generic-extlinux-compatible.enable = false;
            efi.canTouchEfiVariables = false;
            timeout = 0;
            grub = {
              enable = true;
              efiInstallAsRemovable = true;
              efiSupport = true;
              device = "nodev";
            };
          };
        };
        fileSystems = {
          "/" = {
            device = "/dev/disk/by-label/ROOT";
            fsType = "ext4";
            options = [ "noatime" ];
          };
          "/boot" = {
            device = "/dev/disk/by-label/BOOT";
            fsType = "vfat";
          };
        };
        swapDevices = [
          {
            device = "/dev/disk/by-label/SWAP";
          }
          {
            device = "/var/lib/swapfile";
            size = 16 * 1024;
          }
        ];
        time.timeZone = "Europe/Madrid";
        environment.systemPackages = with pkgs; [
          emacs
          git
          rsync
        ];
        services.openssh = {
          enable = true;
          settings = {
            PasswordAuthentication = false;
            KbdInteractiveAuthentication = false;
            PermitRootLogin = "yes";
          };
        };
        users.users = {
          root.openssh.authorizedKeys.keys = [
            pkgs.secrets.personal.publicSshKey
            pkgs.secrets.work.publicSshKey
          ];
          capella = {
            isNormalUser = true;
            extraGroups = [ "wheel" ];
          };
        };
        services.resolved.enable = true;
        nixarr = {
          enable = true;
          jellyfin.enable = true;
          jellyseerr.enable = true;
          lidarr.enable = true;
          readarr.enable = true;
          radarr.enable = true;
          bazarr.enable = true;
          sabnzbd = {
            enable = true;
            guiPort = 8081;
          };
          sonarr.enable = true;
          prowlarr.enable = true;
          transmission = {
            enable = true;
            extraSettings = {
              rpc-host-whitelist-enabled = false;
              rpc-whitelist-enabled = false;
            };
          };
        };
        users.users.${config.util-nixarr.globals.radarr.user}.extraGroups = [ "video" ];
        systemd.tmpfiles.settings."10-radarr".${config.services.radarr.dataDir}.d = lib.mkForce {
          inherit (config.services.radarr) user group;
          mode = "0775";
        };
        sops = {
          defaultSopsFile = ../secrets.yaml;
          age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
          secrets = {
            "hosts/auriga/filestash/admin_password" = {
              owner = config.services.filestash.user;
            };
            "hosts/auriga/syncthing/key" = { };
            "hosts/auriga/syncthing/cert" = { };
            "hosts/auriga/tubo/db/password" = { };
            "hosts/auriga/searxng/secret_key" = { };
            "hosts/auriga/radarr/api_key" = { };
            "hosts/auriga/sonarr/api_key" = { };
            "hosts/auriga/lidarr/api_key" = { };
            "hosts/auriga/readarr/api_key" = { };
            "hosts/auriga/prowlarr/api_key" = { };
            "hosts/auriga/bazarr/api_key" = { };
            "hosts/auriga/jellyfin/api_key" = { };
            "hosts/auriga/jellyseerr/api_key" = { };
            "hosts/auriga/grafana/password" = { };
          };
          templates = {
            "tubo-backend.env".content = ''
              DB_HOST=tubo-db
              DB_NAME=tubo
              DB_USER=tubo
              DB_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
            '';
            "tubo-db.env".content = ''
              POSTGRES_DB=tubo
              POSTGRES_USER=tubo
              POSTGRES_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
            '';
            "searxng.env".content = ''
              SEARXNG_SECRET=${config.sops.placeholder."hosts/auriga/searxng/secret_key"}
            '';
            "homepage.env".content = ''
              HOMEPAGE_VAR_RADARR_API_KEY=${config.sops.placeholder."hosts/auriga/radarr/api_key"}
              HOMEPAGE_VAR_SONARR_API_KEY=${config.sops.placeholder."hosts/auriga/sonarr/api_key"}
              HOMEPAGE_VAR_JELLYFIN_API_KEY=${config.sops.placeholder."hosts/auriga/jellyfin/api_key"}
              HOMEPAGE_VAR_JELLYSEERR_API_KEY=${config.sops.placeholder."hosts/auriga/jellyseerr/api_key"}
              HOMEPAGE_VAR_READARR_API_KEY=${config.sops.placeholder."hosts/auriga/readarr/api_key"}
              HOMEPAGE_VAR_LIDARR_API_KEY=${config.sops.placeholder."hosts/auriga/lidarr/api_key"}
              HOMEPAGE_VAR_PROWLARR_API_KEY=${config.sops.placeholder."hosts/auriga/prowlarr/api_key"}
              HOMEPAGE_VAR_BAZARR_API_KEY=${config.sops.placeholder."hosts/auriga/bazarr/api_key"}
              HOMEPAGE_VAR_GRAFANA_PASSWORD=${config.sops.placeholder."hosts/auriga/grafana/password"}
            '';
          };
        };
        systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";
        services.syncthing = {
          enable = true;
          key = config.sops.secrets."hosts/auriga/syncthing/key".path;
          cert = config.sops.secrets."hosts/auriga/syncthing/cert".path;
          overrideDevices = true;
          overrideFolders = true;
          settings = {
            devices = {
              lyra.id = "M4GMJIA-KU75HGM-BTXRUNS-MVXZQFD-N2YG5KQ-6V2RQHZ-CNORKP5-H2WN6AP";
            };
            folders = {
              documents = {
                path = "~/documents";
                devices = [ "lyra" ];
              };
              pictures = {
                path = "~/pictures";
                devices = [ "lyra" ];
              };
              notes = {
                path = "~/notes";
                devices = [ "lyra" ];
              };
              videos = {
                path = "~/videos";
                devices = [ "lyra" ];
              };
            };
          };
        };
        services.redlib = {
          enable = true;
          port = 8000;
        };
        services.soju = {
          enable = true;
          listen = [
            "irc+insecure://"
            "ws+insecure://0.0.0.0:8080"
          ];
          httpOrigins = [ "*" ];
        };
        services.searx = {
          enable = true;
          redisCreateLocally = true;
          settings = {
            general = {
              debug = false;
            };
            server = {
              port = 8888;
              bind_address = "0.0.0.0";
            };
            search = {
              favicon_resolver = "google";
              autocomplete = "google";
              autocomplete_min = 2;
              method = "GET";
            };
            ui = {
              infinite_scroll = true;
              default_theme = "simple";
            };
            categories_as_tabs = {
              general = { };
              images = { };
              videos = { };
              news = { };
            };
            engines = lib.mapAttrsToList (name: value: { inherit name; } // value) {
              "duckduckgo".disabled = true;
              "qwant".disabled = true;
              "startpage".disabled = false;
              "piped".disabled = true;
              "vimeo".disabled = true;
              "dailymotion".disabled = true;
              "library genesis".disabled = false;
              "reddit".disabled = false;
            };
          };
          environmentFile = config.sops.templates."searxng.env".path;
        };
        systemd.services.homepage-dashboard.serviceConfig.Group = "podman";
        services.homepage-dashboard = {
          enable = true;
          allowedHosts = "*";
          environmentFile = config.sops.templates."homepage.env".path;
          widgets = [
            {
              resources = {
                cpu = true;
                memory = true;
                disk = "/";
                cputemp = true;
                uptime = true;
                expanded = true;
              };
            }
          ];
          docker = {
            auriga-podman = {
              socket = "/var/run/podman/podman.sock";
            };
          };
          settings = {
            target = "_self";
            headerStyle = "boxedWidgets";
            color = "slate";
            disableCollapse = true;
            hideVersion = true;
            disableUpdateCheck = true;
            layout = {
              "Monitoring" = {
                style = "row";
                columns = 2;
              };
              "Download Clients" = {
                style = "row";
                columns = 2;
              };
              "Media Automation" = {
                style = "row";
                columns = 4;
              };
              "Communication" = {
                style = "row";
                columns = 2;
              };
              "Media and Storage" = {
                style = "row";
                columns = 2;
              };
              "Privacy Clients" = {
                style = "row";
                columns = 3;
              };
            };
          };
          services =
            let
              mkHostUrl = port: "http://${config.networking.hostName}:${toString port}";
            in
            [
              {
                "Monitoring" = [
                  {
                    "Grafana" = {
                      icon = "grafana";
                      href = mkHostUrl config.services.grafana.settings.server.http_port;
                      widget = {
                        type = "grafana";
                        url = mkHostUrl config.services.grafana.settings.server.http_port;
                        username = "admin";
                        password = "{{HOMEPAGE_VAR_GRAFANA_PASSWORD}}";
                      };
                    };
                  }
                  {
                    "Adguard Home" = {
                      icon = "adguard-home";
                      href = mkHostUrl config.services.adguardhome.port;
                      widget = {
                        type = "adguard";
                        url = mkHostUrl config.services.adguardhome.port;
                        username = "";
                        password = "";
                      };
                    };
                  }
                ];
              }
              {
                "Communication" = [
                  {
                    "Gamja" = {
                      icon = "irc";
                      href = mkHostUrl 4800;
                    };
                  }
                  {
                    "Movim" = {
                      icon = "https://chat.migalmoreno.com/theme/img/app/128.png";
                      href = "https://chat.migalmoreno.com";
                    };
                  }
                ];
              }
              {
                "Media and Storage" = [
                  {
                    "Filestash" = {
                      icon = "filestash";
                      href = mkHostUrl config.services.filestash.settings.general.port;
                    };
                  }
                  {
                    "Cgit" = {
                      icon = "git";
                      href = mkHostUrl 4040;
                    };
                  }
                  {
                    "Jellyfin" = {
                      icon = "jellyfin";
                      href = mkHostUrl 8096;
                      widget = {
                        type = "jellyfin";
                        url = mkHostUrl 8096;
                        key = "{{HOMEPAGE_VAR_JELLYFIN_API_KEY}}";
                      };
                    };
                  }
                ];
              }
              {
                "Media Automation" = [
                  {
                    "Jellyseerr" = {
                      icon = "jellyseerr";
                      href = mkHostUrl config.services.jellyseerr.port;
                      widget = {
                        type = "jellyseerr";
                        url = mkHostUrl config.services.jellyseerr.port;
                        key = "{{HOMEPAGE_VAR_JELLYSEERR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Radarr" = {
                      icon = "radarr";
                      href = mkHostUrl 7878;
                      widget = {
                        type = "radarr";
                        url = mkHostUrl 7878;
                        key = "{{HOMEPAGE_VAR_RADARR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Sonarr" = {
                      icon = "sonarr";
                      href = mkHostUrl 8989;
                      widget = {
                        type = "sonarr";
                        url = mkHostUrl 8989;
                        key = "{{HOMEPAGE_VAR_SONARR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Readarr" = {
                      icon = "readarr";
                      href = mkHostUrl 8787;
                      widget = {
                        type = "readarr";
                        url = mkHostUrl 8787;
                        key = "{{HOMEPAGE_VAR_READARR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Lidarr" = {
                      icon = "lidarr";
                      href = mkHostUrl 8686;
                      widget = {
                        type = "lidarr";
                        url = mkHostUrl 8686;
                        key = "{{HOMEPAGE_VAR_LIDARR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Bazarr" = {
                      icon = "bazarr";
                      href = mkHostUrl config.services.bazarr.listenPort;
                      widget = {
                        type = "bazarr";
                        url = mkHostUrl config.services.bazarr.listenPort;
                        key = "{{HOMEPAGE_VAR_BAZARR_API_KEY}}";
                      };
                    };
                  }
                  {
                    "Prowlarr" = {
                      icon = "prowlarr";
                      href = mkHostUrl 9696;
                      widget = {
                        type = "prowlarr";
                        url = mkHostUrl 9696;
                        key = "{{HOMEPAGE_VAR_PROWLARR_API_KEY}}";
                      };
                    };
                  }
                ];
              }
              {
                "Privacy Clients" = [
                  {
                    "Tubo" = {
                      icon = mkHostUrl "8083/icons/tubo.svg";
                      href = mkHostUrl 8083;
                      server = "auriga-podman";
                      container = "tubo-backend";
                      showStats = true;
                    };
                  }
                  {
                    "Redlib" = {
                      icon = "redlib";
                      href = mkHostUrl config.services.redlib.port;
                    };
                  }
                  {
                    "SearXNG" = {
                      icon = "searxng";
                      href = mkHostUrl config.services.searx.settings.server.port;
                    };
                  }
                ];
              }
              {
                "Download Clients" = [
                  {
                    "Transmission" = {
                      icon = "transmission";
                      href = mkHostUrl config.nixarr.transmission.uiPort;
                      widget = {
                        type = "transmission";
                        url = mkHostUrl config.nixarr.transmission.uiPort;
                      };
                    };
                  }
                  {
                    "SABnzbd" = {
                      icon = "sabnzbd";
                      href = mkHostUrl config.nixarr.sabnzbd.guiPort;
                    };
                  }
                ];
              }
            ];
        };
        services.adguardhome = {
          enable = true;
          port = 3003;
          settings = { };
        };
        services.grafana = {
          enable = true;
          settings = {
            server = {
              http_addr = "0.0.0.0";
              http_port = 3030;
            };
          };
        };
        services.prometheus = {
          enable = true;
          exporters = {
            node = {
              enable = true;
              enabledCollectors = [ "systemd" ];
            };
          };
          scrapeConfigs = [
            {
              job_name = "node";
              static_configs = [
                {
                  targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ];
                }
              ];
            }
          ];
        };
        services.webdav = {
          enable = true;
          user = "syncthing";
          settings = {
            address = "0.0.0.0";
            port = 6065;
            directory = "/var/lib/syncthing";
            permissions = "CRUD";
            debug = true;
            cors = {
              enabled = true;
              credentials = true;
              exposed_headers = [
                "Content-Length"
                "Content-Range"
                "Origin"
                "X-Requested-With"
                "Content-Type"
                "Accept"
              ];
            };
          };
        };
        services.filestash = {
          enable = true;
          settings = {
            general = {
              port = 8334;
              fork_button = false;
              display_hidden = true;
              filepage_default_view = "list";
              filepage_default_sort = "name";
            };
            auth.admin_file = config.sops.secrets."hosts/auriga/filestash/admin_password".path;
            connections = [
              {
                label = "webdav";
                type = "webdav";
                url = with config.services.webdav.settings; "http://${address}:${toString port}";
                username = "";
                password = "";
              }
            ];
          };
        };
        services.nginx = {
          enable = true;
          enableReload = true;
          recommendedOptimisation = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          virtualHosts = {
            "git.migalmoreno.com" = {
              listen = [
                {
                  addr = "0.0.0.0";
                  port = 4040;
                }
              ];
            };
            "irc.auriga" = {
              root = "${pkgs.gamja}";
              locations."= /config.json".extraConfig = ''
                alias ${pkgs.writeText "gamja-config.json" ''
                  {
                    "server": {
                      "url": "http://auriga:8080",
                      "autojoin": [],
                      "auth": "optional",
                      "nick": "migalmoreno",
                      "autoconnect": false,
                      "ping": 0
                    }
                  }
                ''};
              '';
              listen = [
                {
                  addr = "0.0.0.0";
                  port = 4800;
                }
              ];
            };
            "home.auriga" = {
              listen = [
                {
                  addr = "0.0.0.0";
                  port = 80;
                }
              ];
              locations."/".proxyPass = "http://localhost:8082";
            };
          };
        };
        virtualisation.podman = {
          enable = true;
          defaultNetwork.settings = {
            dns_enabled = true;
          };
          autoPrune = {
            enable = true;
            flags = [ "--all" ];
          };
        };
        systemd.services.podman-auto-update = {
          enable = false;
          wantedBy = [ "multi-user.target" ];
        };
        systemd.timers.podman-auto-update = {
          enable = false;
          timerConfig = {
            OnCalendar = "daily";
            RandomizedDelaySec = 900;
            Persistent = true;
          };
        };
        virtualisation.oci-containers.containers = {
          tubo-frontend = {
            image = "migalmoreno/tubo-frontend";
            ports = [ "8083:8080" ];
            environment = {
              BACKEND_URL = "http://auriga:3000";
            };
            extraOptions = [ "--pull=newer" ];
          };
          tubo-backend = {
            image = "migalmoreno/tubo-backend";
            ports = [ "3000:3000" ];
            extraOptions = [ "--pull=newer" ];
            dependsOn = [ "tubo-db" ];
            environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
          };
          tubo-bg-helper = {
            image = "migalmoreno/tubo-bg-helper";
            ports = [ "3005:3005" ];
            extraOptions = [ "--pull=newer" ];
          };
          tubo-db = {
            image = "postgres:16-alpine";
            ports = [ "5432:5432" ];
            volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
            environmentFiles = [ config.sops.templates."tubo-db.env".path ];
          };
        };
        systemd.services.pgweb = {
          enable = false;
          description = "pgweb";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.pgweb}/bin/pgweb";
            Restart = "on-failure";
            KillMode = "mixed";
          };
          environment.PGWEB_DATABASE_URL = "postgres://tubo:tubo@localhost:5432/tubo";
        };
        system.stateVersion = "24.05";
      }
    )
  ];
}
