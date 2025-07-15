{
  config,
  inputs,
  lib,
  ...
}:

{
  imports = [ inputs.nixarr.nixosModules.default ];
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
  boot = {
    kernelModules = [ "v4l2loopback" ];
    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  };
  users.users.${config.util-nixarr.globals.radarr.user}.extraGroups = [ "video" ];
  systemd.tmpfiles.settings."10-radarr".${config.services.radarr.dataDir}.d = lib.mkForce {
    inherit (config.services.radarr) user group;
    mode = "0775";
  };
  sops = {
    secrets = {
      "hosts/auriga/radarr/api_key" = { };
      "hosts/auriga/sonarr/api_key" = { };
      "hosts/auriga/lidarr/api_key" = { };
      "hosts/auriga/readarr/api_key" = { };
      "hosts/auriga/prowlarr/api_key" = { };
      "hosts/auriga/bazarr/api_key" = { };
      "hosts/auriga/jellyfin/api_key" = { };
      "hosts/auriga/jellyseerr/api_key" = { };
    };
    templates."homepage.env".content = ''
      HOMEPAGE_VAR_RADARR_API_KEY=${config.sops.placeholder."hosts/auriga/radarr/api_key"}
      HOMEPAGE_VAR_SONARR_API_KEY=${config.sops.placeholder."hosts/auriga/sonarr/api_key"}
      HOMEPAGE_VAR_JELLYFIN_API_KEY=${config.sops.placeholder."hosts/auriga/jellyfin/api_key"}
      HOMEPAGE_VAR_JELLYSEERR_API_KEY=${config.sops.placeholder."hosts/auriga/jellyseerr/api_key"}
      HOMEPAGE_VAR_READARR_API_KEY=${config.sops.placeholder."hosts/auriga/readarr/api_key"}
      HOMEPAGE_VAR_LIDARR_API_KEY=${config.sops.placeholder."hosts/auriga/lidarr/api_key"}
      HOMEPAGE_VAR_PROWLARR_API_KEY=${config.sops.placeholder."hosts/auriga/prowlarr/api_key"}
      HOMEPAGE_VAR_BAZARR_API_KEY=${config.sops.placeholder."hosts/auriga/bazarr/api_key"}
    '';
  };
  profiles.homepage.services = {
    "Media and Storage".widgets = [
      {
        "Jellyfin" = {
          icon = "jellyfin";
          href = "http://${config.networking.hostName}:8096";
          widget = {
            type = "jellyfin";
            url = "http://${config.networking.hostName}:8096";
            key = "{{HOMEPAGE_VAR_JELLYFIN_API_KEY}}";
          };
        };
      }
    ];
    "Media Automation" = {
      layout = {
        style = "row";
        columns = 4;
      };
      widgets = [
        {
          "Jellyseerr" = {
            icon = "jellyseerr";
            href = "http://${config.networking.hostName}:${toString config.services.jellyseerr.port}";
            widget = {
              type = "jellyseerr";
              url = "http://${config.networking.hostName}:${toString config.services.jellyseerr.port}";
              key = "{{HOMEPAGE_VAR_JELLYSEERR_API_KEY}}";
            };
          };
        }
        {
          "Radarr" = {
            icon = "radarr";
            href = "http://${config.networking.hostName}:7878";
            widget = {
              type = "radarr";
              url = "http://${config.networking.hostName}:7878";
              key = "{{HOMEPAGE_VAR_RADARR_API_KEY}}";
            };
          };
        }
        {
          "Sonarr" = {
            icon = "sonarr";
            href = "http://${config.networking.hostName}:8989";
            widget = {
              type = "sonarr";
              url = "http://${config.networking.hostName}:8989";
              key = "{{HOMEPAGE_VAR_SONARR_API_KEY}}";
            };
          };

        }
        {
          "Readarr" = {
            icon = "readarr";
            href = "http://${config.networking.hostName}:8787";
            widget = {
              type = "readarr";
              url = "http://${config.networking.hostName}:8787";
              key = "{{HOMEPAGE_VAR_READARR_API_KEY}}";
            };
          };
        }
        {
          "Lidarr" = {
            icon = "lidarr";
            href = "http://${config.networking.hostName}:8686";
            widget = {
              type = "lidarr";
              url = "http://${config.networking.hostName}:8686";
              key = "{{HOMEPAGE_VAR_LIDARR_API_KEY}}";
            };
          };
        }
        {
          "Bazarr" = {
            icon = "bazarr";
            href = "http://${config.networking.hostName}:${toString config.services.bazarr.listenPort}";
            widget = {
              type = "bazarr";
              url = "http://${config.networking.hostName}:${toString config.services.bazarr.listenPort}";
              key = "{{HOMEPAGE_VAR_BAZARR_API_KEY}}";
            };
          };
        }
        {
          "Prowlarr" = {
            icon = "prowlarr";
            href = "http://${config.networking.hostName}:9696";
            widget = {
              type = "prowlarr";
              url = "http://${config.networking.hostName}:9696";
              key = "{{HOMEPAGE_VAR_PROWLARR_API_KEY}}";
            };
          };
        }
      ];
    };
    "Download Clients" = {
      layout = {
        style = "row";
        columns = 2;
      };
      widgets = [
        {
          "Transmission" = {
            icon = "transmission";
            href = "http://${config.networking.hostName}:${toString config.nixarr.transmission.uiPort}";
            widget = {
              type = "transmission";
              url = "http://${config.networking.hostName}:${toString config.nixarr.transmission.uiPort}";
            };
          };
        }
        {
          "SABnzbd" = {
            icon = "sabnzbd";
            href = "http://${config.networking.hostName}:${toString config.nixarr.sabnzbd.guiPort}";
          };
        }
      ];
    };
  };
}
