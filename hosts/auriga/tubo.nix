{ config, ... }:

{
  sops = {
    secrets."hosts/auriga/tubo/db/password" = { };
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
      "pgweb.env".content = ''
        PGWEB_DATABASE_URL=postgres://tubo:${
          config.sops.placeholder."hosts/auriga/tubo/db/password"
        }@tubo-db:5432/tubo?sslmode=disable
      '';
    };
  };
  services.resolved.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
  virtualisation.oci-containers.containers = {
    tubo-frontend = {
      image = "migalmoreno/tubo-frontend";
      ports = [ "8083:8080" ];
      environment = {
        BACKEND_URL = "http://auriga:3000";
      };
    };
    tubo-backend = {
      image = "migalmoreno/tubo-backend";
      ports = [ "3000:3000" ];
      dependsOn = [ "tubo-db" ];
      environmentFiles = [ config.sops.templates."tubo-backend.env".path ];
    };
    tubo-bg-helper = {
      image = "migalmoreno/tubo-bg-helper";
      ports = [ "3005:3005" ];
    };
    tubo-db = {
      image = "postgres:16-alpine";
      ports = [ "5432:5432" ];
      volumes = [ "/var/local/db/tubo:/var/lib/postgresql/data" ];
      environmentFiles = [ config.sops.templates."tubo-db.env".path ];
    };
    pgweb = {
      image = "sosedoff/pgweb";
      ports = [ "8088:8081" ];
      environmentFiles = [ config.sops.templates."pgweb.env".path ];
      dependsOn = [ "tubo-db" ];
    };
  };
  systemd.tmpfiles.settings = {
    "10-tubo"."/var/local/db/tubo".d = {
      user = "root";
      group = "root";
      mode = "0755";
    };
  };
  networking.firewall.allowedTCPPorts = [ 3000 ];
}
