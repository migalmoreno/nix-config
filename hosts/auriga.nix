{ inputs, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    nixos-hardware.nixosModules.raspberry-pi-4
    nixarr.nixosModules.default
    sops-nix.nixosModules.sops
    ordenada.nixosModules.ordenada
    filestash-nix.nixosModules.filestash
    ../services/cgit.nix
    ../services/nix.nix
    ../services/whoogle-search.nix
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        hardware = {
          enableRedistributableFirmware = true;
          raspberry-pi."4" = {
            apply-overlays-dtmerge.enable = true;
            fkms-3d.enable = true;
          };
        };
        hardware.graphics.enable = true;
        networking.hostName = "auriga";
        networking.interfaces.wlan0.useDHCP = true;
        networking.firewall.enable = true;
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
        ordenada = {
          users.capella = { };
          features = {
            userInfo.username = "capella";
            home.enable = true;
            bash.enable = true;
            networking.enable = true;
            ssh = {
              enable = true;
              rootAuthorizedKeys = [
                pkgs.secrets.personal.publicSshKey
                pkgs.secrets.work.publicSshKey
              ];
            };
            tailscale.enable = true;
            nix.enable = true;
            docker.enable = true;
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
          };
          templates."tubo-backend.env".content = ''
            DB_HOST=tubo-db
            DB_NAME=tubo
            DB_USER=tubo
            DB_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
          '';
          templates."tubo-db.env".content = ''
            POSTGRES_DB=tubo
            POSTGRES_USER=tubo
            POSTGRES_PASSWORD=${config.sops.placeholder."hosts/auriga/tubo/db/password"}
          '';
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
        networking.firewall.allowedTCPPorts = [
          6667
          3000
        ];
        services.homepage-dashboard = {
          enable = true;
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
          settings = {
            target = "_self";
            headerStyle = "clean";
            color = "neutral";
          };
          services = [
            {
              "Monitoring" = [
                {
                  "Grafana" = {
                    icon = "grafana";
                    href = "http://${config.networking.hostName}:${toString config.services.grafana.settings.server.http_port}";
                  };
                }
                {
                  "Adguard Home" = {
                    icon = "adguard-home";
                    href = "http://${config.networking.hostName}:${toString config.services.adguardhome.port}";
                  };
                }
              ];
            }
            {
              "Communication" = [
                {
                  "Gamja" = {
                    icon = "irc";
                    href = "http://${config.networking.hostName}:4800";
                  };
                }
              ];
            }
            {
              "Privacy Clients" = [
                {
                  "Tubo" = {
                    icon = "http://${config.networking.hostName}:8083/icons/tubo.svg";
                    href = "http://${config.networking.hostName}:8083";
                  };
                }
                {
                  "Redlib" = {
                    icon = "redlib";
                    href = "http://${config.networking.hostName}:${toString config.services.redlib.port}";
                  };
                }
                {
                  "Whoogle" = {
                    icon = "whoogle";
                    href = "http://${config.networking.hostName}:${toString config.services.whoogle-search.port}";
                  };
                }
              ];
            }
            {
              "Media and Storage" = [
                {
                  "Jellyfin" = {
                    icon = "jellyfin";
                    href = "http://${config.networking.hostName}:8096";
                  };
                }
                {
                  "Filestash" = {
                    icon = "filestash";
                    href = "http://${config.networking.hostName}:${toString config.services.filestash.settings.general.port}";
                  };
                }
                {
                  "Cgit" = {
                    icon = "git";
                    href = "http://${config.networking.hostName}:4040";
                  };
                }
              ];
            }
            {
              "Media Automation" = [
                {
                  "Jellyseerr" = {
                    icon = "jellyseerr";
                    href = "http://${config.networking.hostName}:${toString config.services.jellyseerr.port}";
                  };
                }
                {
                  "Radarr" = {
                    icon = "radarr";
                    href = "http://${config.networking.hostName}:7878";
                  };
                }
                {
                  "Sonarr" = {
                    icon = "sonarr";
                    href = "http://${config.networking.hostName}:8989";
                  };
                }
                {
                  "Readarr" = {
                    icon = "readarr";
                    href = "http://${config.networking.hostName}:8787";
                  };
                }
                {
                  "Lidarr" = {
                    icon = "lidarr";
                    href = "http://${config.networking.hostName}:8686";
                  };
                }
                {
                  "Prowlarr" = {
                    icon = "prowlarr";
                    href = "http://${config.networking.hostName}:9696";
                  };
                }
                {
                  "Bazarr" = {
                    icon = "bazarr";
                    href = "http://${config.networking.hostName}:${toString config.services.bazarr.listenPort}";
                  };
                }
              ];
            }
            {
              "Download Clients" = [
                {
                  "Transmission" = {
                    icon = "transmission";
                    href = "http://${config.networking.hostName}:${toString config.nixarr.transmission.uiPort}";
                  };
                }
                {
                  "SABnzbd" = {
                    icon = "sabnzbd";
                    href = "http://${config.networking.hostName}:${toString config.nixarr.sabnzbd.guiPort}";
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
