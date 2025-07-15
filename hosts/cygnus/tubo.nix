{ config, ... }:

{
  sops = {
    secrets = {
      "hosts/cygnus/tubo/db/password" = { };
    };
    templates = {
      "tubo-backend.env".content = ''
        DB_HOST=tubo-db
        DB_NAME=tubo
        DB_USER=tubo
        DB_PASSWORD=${config.sops.placeholder."hosts/cygnus/tubo/db/password"}
      '';
      "tubo-db.env".content = ''
        POSTGRES_DB=tubo
        POSTGRES_USER=tubo
        POSTGRES_PASSWORD=${config.sops.placeholder."hosts/cygnus/tubo/db/password"}
      '';
    };
  };
  services.resolved.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
  virtualisation.oci-containers.containers = {
    tubo-frontend = {
      image = "migalmoreno/tubo-frontend";
      ports = [ "8080:8080" ];
      environment = {
        BACKEND_URL = "https://api.tubo.media";
      };
      extraOptions = [ "--pull=newer" ];
    };
    tubo-backend = {
      image = "migalmoreno/tubo-backend";
      ports = [ "3000:3000" ];
      extraOptions = [
        "--pull=newer"
      ];
      dependsOn = [
        "tubo-db"
      ];
      environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
    };
    tubo-bg-helper = {
      image = "migalmoreno/tubo-bg-helper";
      ports = [ "3005:3005" ];
      extraOptions = [
        "--pull=newer"
      ];
    };
    tubo-db = {
      image = "postgres:16-alpine";
      volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."tubo-db.env".path ];
    };
  };
  services.nginx.virtualHosts = with config.profiles.nginx.globals; {
    "tubo.media" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = crawlersBlock;
      locations = {
        "/" = {
          proxyPass = "http://localhost:8080";
          extraConfig = ''
            limit_req zone=ip burst=20 nodelay;
          '';
        };
        "/robots.txt" = robotsTxt;
      };
    };
    "api.tubo.media" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = crawlersBlock;
      locations = {
        "/" = {
          proxyPass = "http://localhost:3000";
          extraConfig = ''
            limit_req zone=ip burst=20 nodelay;
          '';
        };
        "/robots.txt" = robotsTxt;
      };
    };
  };
}
