{ config, ... }:

{
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
              };
            }
            {
              "Adguard Home" = {
                icon = "adguard-home";
                href = mkHostUrl config.services.adguardhome.port;
              };
            }
            {
              "GoAccess" = {
                icon = "goaccess";
                href = "http://cygnus:8081";
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
  systemd.services.homepage-dashboard.serviceConfig.Group = "podman";
  sops.templates."homepage.env".content = ''
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
  services.nginx.virtualHosts."home.auriga" = {
    listen = [
      {
        addr = "0.0.0.0";
        port = 80;
      }
    ];
    locations."/".proxyPass = "http://localhost:8082";
  };
}
