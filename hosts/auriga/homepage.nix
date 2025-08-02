{ config, ... }:

{
  imports = [
    ../../profiles/homepage.nix
  ];
  profiles.homepage = {
    environmentFile = config.sops.templates."homepage.env".path;
    widgets = [
      {
        search = {
          provider = "custom";
          url = "http://auriga:8888/search?q=";
          suggestionUrl = "http://auriga:8888/autocompleter?q=";
          showSearchSuggestions = true;
          target = "_self";
          focus = true;
        };
      }
    ];
    layout = [
      {
        "Download Clients".style = "column";
      }
      {
        "Communication".style = "column";
      }
      {
        "Media Automation" = {
          style = "row";
          columns = 4;
        };
      }
      {
        "Media and Storage".style = "column";
      }
      {
        "Monitoring".style = "column";
      }
    ];
    services = {
      "Communication" = [
        {
          "Movim" = {
            icon = "https://chat.migalmoreno.com/theme/img/app/128.png";
            href = "https://chat.migalmoreno.com";
          };
        }
      ];
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
        {
          "GoAccess" = {
            icon = "goaccess";
            href = "http://cygnus:8081";
          };
        }
      ];
    };
  };
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
