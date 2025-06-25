{ config, ... }:

{
  sops = {
    secrets = {
      "hosts/cygnus/gluetun/config" = { };
      "hosts/cygnus/tubo/db/password" = { };
    };
    templates = {
      "gluetun.env".content = config.sops.placeholder."hosts/cygnus/gluetun/config";
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
        "--network=container:gluetun"
        "--pull=newer"
      ];
      dependsOn = [
        "gluetun"
        "tubo-db"
      ];
      environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
    };
    tubo-bg-helper = {
      image = "migalmoreno/tubo-bg-helper";
      ports = [ "3005:3005" ];
      extraOptions = [
        "--network=container:gluetun"
        "--pull=newer"
      ];
      dependsOn = [ "gluetun" ];
    };
    tubo-db = {
      image = "postgres:16-alpine";
      extraOptions = [
        "--network=container:gluetun"
      ];
      dependsOn = [ "gluetun" ];
      volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."tubo-db.env".path ];
    };
    gluetun = {
      image = "qmcgaw/gluetun";
      extraOptions = [
        "--cap-add=NET_ADMIN"
        "--device=/dev/net/tun:/dev/net/tun"
      ];
      ports = [
        "3000:3000"
        "3005:3005"
      ];
      environmentFiles = [ config.sops.templates."gluetun.env".path ];
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
